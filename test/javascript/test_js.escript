#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./deps/*/ebin -pa ./apps/*/ebin -pa ./test/etap

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% Test replication of documents with many leaf revisions.
% Motivated by COUCHDB-1340 and other similar issues where a document
% GET with a too long ?open_revs revision list doesn't work due to
% maximum web server limits for the HTTP request path.
%
%
-module(test_js).

-export([main/1]).

builddir() ->
    Current = filename:dirname(escript:script_name()),
    filename:absname(filename:join([Current, "..", ".."])).

srcdir() ->
    filename:join([builddir(), "apps"]).

depsdir() ->
    filename:join([builddir(), "deps"]).

testdir() ->
    filename:join([builddir(), "test", "out"]).

scriptdir() ->
    filename:join([testdir(), "share", "www", "script"]).


script_file(Name) ->
    filename:join([scriptdir(), Name]).

js_test_file(Name) ->
    filename:join([builddir(), "test", "javascript", Name]).


config_files() ->
    [
        filename:join([testdir(), "couch_test.ini"]),
        filename:join([testdir(), "local.ini"])
    ].



%%
%% Given a list of key value pairs, for each string value attempt to
%% render it using Dict as the context. Storing the result in Dict as Key.
%%
resolve_variables([], Dict) ->
    Dict;
resolve_variables([{Key, Value0} | Rest], Dict) when is_integer(Value0) ->
    Value = render(list_to_binary(integer_to_list(Value0)), Dict),
    resolve_variables(Rest, dict:store(Key, Value, Dict));
resolve_variables([{Key, Value0} | Rest], Dict) when is_list(Value0) ->
    Value = render(list_to_binary(Value0), Dict),
    resolve_variables(Rest, dict:store(Key, Value, Dict));
resolve_variables([{Key, {list, Dicts}} | Rest], Dict) when is_list(Dicts) ->
    %% just un-tag it so mustache can use it
    resolve_variables(Rest, dict:store(Key, Dicts, Dict));
resolve_variables([_Pair | Rest], Dict) ->
    resolve_variables(Rest, Dict).

%%
%% Render a binary to a string, using mustache and the specified context
%%

render(Bin, Context) ->
    %% Be sure to escape any double-quotes before rendering...
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    mustache:render(Str1, Context).


init_config() ->
    {ok, Vars} = file:consult(filename:join([builddir(), "test",
                                             "vars.config"])),

    Vars1 = resolve_variables(Vars, dict:from_list([{testdir, testdir()}])),

    %% create test config
    {ok, Bin} = file:read_file(filename:join([builddir(), "etc",
                                              "couchdb", "couch.ini"])),

    Rendered = render(Bin, Vars1),
    file:write_file(filename:join([testdir(), "couch_test.ini"]),
                     Rendered).


init_code_path() ->
    lists:foreach(fun(Name) ->
                code:add_patha(filename:join([depsdir(), Name, "ebin"]))
        end, filelib:wildcard("*", depsdir())),

    lists:foreach(fun(Name) ->
                code:add_patha(filename:join([srcdir(), Name, "ebin"]))
        end, filelib:wildcard("*", srcdir())),

    code:add_patha(filename:join([builddir(), "test", "etap"])),

    %% init config
    init_config().

start_couch(Verbose) ->
    ok = init_code_path(),
    IniFiles = config_files(),

    application:load(sasl),
    %% disable sasl logging
    application:set_env(sasl, errlog_type, error),
    application:set_env(sasl, sasl_error_logger, false),

    %% start couch
    application:load(couch),
    application:set_env(couch, config_files, IniFiles),
    couch_util:start_app_deps(couch),
    application:start(couch),
    %% set couch log level
    couch_config:set("log", "level", atom_to_list(Verbose), false),

    couch_util:start_app_deps(couch_httpd),
    application:start(couch_httpd),
    couch_util:start_app_deps(couch_replicator),
    application:start(couch_replicator).

stop_couch() ->
    application:stop(couch_replicator),
    application:stop(couch_httpd),
    application:stop(couch),
    application:stop(os_mon).

restart_couch(Verbose) ->
    stop_couch(),
    start_couch(Verbose).

exec_loop(Port, Verbose, Acc) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            ok;
        {Port, {data, {eol, "restart"}}} ->
            restart_couch(Verbose),
            exec_loop(Port, Verbose, Acc);
        {Port, {data, {eol, Line}}} ->
            exec_loop(Port, Verbose, Acc ++ "\n" ++ Line);
        {Port, {data, {noeol, Verbose, Line}}} ->
            exec_loop(Port, Verbose, Acc ++ Line);
        {Port, {exit_status, _}} ->
            {error, Acc}
    end.

exec(Path, Verbose) ->
    COUCHJS = filename:join([builddir(), "apps", "couch", "priv",
                             "couchjs"]),
    CouchUri = filename:join([testdir(), "data", "couch.uri"]),
    Cmd = string:join([COUCHJS, "-H", "-u", CouchUri,
                       script_file("json2.js"),
                       script_file("sha1.js"),
                       script_file("oauth.js"),
                       script_file("couch.js"),
                       script_file("replicator_db_inc.js"),
                       script_file("couch_test_runner.js"),
                       js_test_file("couch_http.js"),
                       js_test_file("test_setup.js"),
                       Path,
                       js_test_file("cli_runner.js")], " "),

    PortSettings = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout,
                    hide],

    io:format("~s ... testing~n", [filename:basename(Path)]),
    Port = open_port({spawn, Cmd}, PortSettings),
    Result = case exec_loop(Port, Verbose, "") of
        ok ->
            io:format("~s ... ok~n", [filename:basename(Path)]),
            ok;
        {error, Output} ->
            io:format("~s ... fail~n", [filename:basename(Path)]),
            io:format("javascript traceback:~n~s~n", [Output]),
            fail
    end,

    Result.

test(TestDir, Files, Verbose) ->
    start_couch(Verbose),
    timer:sleep(1000),

    io:format("==> run javascript tests.~n~n", []),
    {Failed, Success} = lists:foldl(fun(Name, {FAILs, OKs}) ->
                Path = filename:join([TestDir, Name]),
                Result = exec(Path, Verbose),
                case Result of
                    ok-> {FAILs, [{Name, ok} | OKs]};
                    _ -> {[{Name, fail} | FAILs], OKs}
                end

        end, {[], []}, Files),

    NFailed = length(Failed),
    NSuccess = length(Success),
    Count = NFailed + NSuccess,

    io:format("~n==> javascript tests results.~n~n", []),
    lists:foreach(fun({Name, Status}) ->
                io:format("~s ... ~s~n", [Name, Status])
        end, lists:usort(Failed ++ Success)),

    case NFailed of
        0 ->
            io:format("~nAll tests successful.~nTests: ~p~n", [Count]),
            halt(0);
        _ ->
            io:format("~n~p/~p tests failed~n", [NFailed, Count]),
            halt(1)
    end.

main([]) ->
    TestDir = filename:join([scriptdir(), "test"]),
    test(TestDir, filelib:wildcard("*.js", TestDir), none);
main(["-v", File | _]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)], info);
main(["-vv", File | _]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)], debug);
main([File |_]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)], none).
