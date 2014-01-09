BASE_DIR = $(shell pwd)
ERLC ?= $(shell which erl)
ESCRIPT ?= $(shell which escript)
REBAR ?= $(BASE_DIR)/rebar
OVERLAY_VARS ?=

$(if $(ERLC),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

$(if $(ESCRIPT),,$(warning "Warning: No escript found in your path, this will probably not work"))

.PHONY: rel deps rebar

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

all: deps compile

compile:
	@$(REBAR) compile

deps: rebar
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

distclean: clean rebarclean relclean

generate:
	@$(REBAR) generate $(OVERLAY_VARS)

rel: generate

relclean:
	@rm -rf rel/apache-couchdb

rebar:
	@(test ! -e $(BASE_DIR)/src/support/rebar/rebar && \
		echo "==> build rebar" && \
		cd $(BASE_DIR)/src/support/rebar && \
		$(ESCRIPT) bootstrap || true)
	@cp $(BASE_DIR)/src/support/rebar/rebar $(BASE_DIR)/rebar

rebarclean:
	@(cd $(BASE_DIR)/support/rebar/rebar && \
		rm -rf rebar ebin/*.beam inttest/rt.work rt.work .test)

.PHONY: rebar
