BASE_DIR = $(shell pwd)
ERLANG_BIN = $(shell dirname $(shell which erl))
REBAR ?= $(BASE_DIR)/rebar
OVERLAY_VARS ?=

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

.PHONY: rel deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean relclean

generate:
	./rebar generate $(OVERLAY_VARS)

rel: deps compile generate

relclean:
	rm -rf rel/apache-couchdb

