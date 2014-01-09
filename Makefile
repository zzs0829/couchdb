BASE_DIR = $(shell pwd)
ERLANG_BIN = $(shell dirname $(shell which erl))
ESCRIPT = $(shell which escript)
GIT_BIN = $(shell dirname $(shell which git))
REBAR ?= $(BASE_DIR)/rebar
OVERLAY_VARS ?=

$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

$(if $(ESCRIPT),,$(warning "Warning: No escript found in your path, this will probably not work"))

$(if $(GIT_BIN),,$(warning "Warning: No Git found in your path, this will probably not work"))


.PHONY: rel deps

COUCHDB_STATIC=1
ifeq ($(libs), shared)
	COUCHDB_STATIC=0
endif
export COUCHDB_STATIC

USE_STATIC_ICU=0
ifeq ($(icu), static)
	USE_STATIC_ICU=1
endif
export USE_STATIC_ICU

all: rebar deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

clean: rebarclean
	@./rebar clean

distclean: clean relclean

generate:
	@./rebar generate $(OVERLAY_VARS)

rel: deps compile generate

relclean:
	@rm -rf rel/apache-couchdb

rebar:
	@(test ! -e $(BASE_DIR)/src/support/rebar/rebar && \
		echo "==> build rebar" && \
		cd $(BASE_DIR)/src/support/rebar && \
		$(ESCRIPT) bootstrap)

	cp $(BASE_DIR)/src/support/rebar/rebar $(BASE_DIR)/rebar

rebarclean:
	@(cd $(BASE_DIR)/support/rebar/rebar && \
		rm -rf rebar ebin/*.beam inttest/rt.work rt.work .test)

.PHONY: rebar
