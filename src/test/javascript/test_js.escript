#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/deps/*/ebin -pa ./src/apps/*/ebin -pa ./src/test/etap

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

start_couch() ->
    ok = init_code_path(),
    IniFiles = config_files(),
    application:load(couch),
    application:set_env(couch, config_files, IniFiles),
    couch_util:start_app_deps(couch),
    application:start(couch),
    couch_util:start_app_deps(couch_httpd),
    application:start(couch_httpd),
    couch_util:start_app_deps(couch_replicator),
    application:start(couch_replicator).

stop_couch() ->
    application:stop(couch_replicator),
    application:stop(couch_httpd),
    application:stop(couch).

restart_couch() ->
    stop_couch(),
    timer:sleep(1000),
    start_couch().

exec(Path) ->
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

    Resp = os:cmd(Cmd),
    Lines = string:tokens(Resp, "\n"),
    Result = lists:foldr(fun
                ("restart", Acc) ->
                    restart_couch(),
                    Acc;
                ("OK", _Acc) ->
                    ok;
                (_, Acc) ->
                    Acc
            end, fail, Lines),
    io:format("~s ... ~s~n", [filename:basename(Path), Result]),
    Result.



test(TestDir, Files) ->
    start_couch(),
    timer:sleep(1000),

    io:format("==> run javascript tests.~n~n", []),
    {Failed, Success} = lists:foldl(fun(Name, {FAILs, OKs}) ->
                Path = filename:join([TestDir, Name]),
                Result = exec(Path),
                case Result of
                    ok-> {FAILs, [Name | OKs]};
                    _ -> {[Name | FAILs], OKs}
                end

        end, {[], []}, Files),

    NFailed = length(Failed),
    NSuccess = length(Success),
    Count = NFailed + NSuccess,

    case NFailed of
        0 ->
            io:format("~nAll tests successful.~nTests: ~p~n", [Count]);
        _ ->
            io:format("~n~p/~p tests failed~n", [NFailed, Count])
    end,

    stop_couch().

main([]) ->
    TestDir = filename:join([scriptdir(), "test"]),
    test(TestDir, filelib:wildcard("*.js", TestDir));
main([File |_]) ->
    Dir = filename:absname(filename:dirname(File)),
    test(Dir, [filename:basename(File)]).
