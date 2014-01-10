BASE_DIR = $(shell pwd)
ERLC ?= $(shell which erlc)
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

check: test testjs

#
# rebar
#

rebar:
	@(test ! -e $(BASE_DIR)/src/support/rebar/rebar && \
		echo "==> build rebar" && \
		cd $(BASE_DIR)/src/support/rebar && \
		$(ESCRIPT) bootstrap || true)
	@cp $(BASE_DIR)/src/support/rebar/rebar $(BASE_DIR)/rebar

rebarclean:
	@(cd $(BASE_DIR)/support/rebar/rebar && \
		rm -rf rebar ebin/*.beam inttest/rt.work rt.work .test)

#
# TESTS
#
COUCHDB_ETAP_DIR=$(BASE_DIR)/src/test/etap
export COUCHDB_ETAP_DIR

ERL_LIBS=$(BASE_DIR)/src/deps:$(BASE_DIR)/src/apps:$(BASE_DIR)/src/test/etap
export ERL_LIBS

test: testbuild
	prove $(COUCHDB_ETAP_DIR)/*.t
	prove $(BASE_DIR)/src/apps/couch_mrview/test/*.t
	prove $(BASE_DIR)/src/apps/couch_replicator/test/*.t

verbose-test: testbuild
	prove -v $(COUCHDB_ETAP_DIR)/*.t
	prove -v $(BASE_DIR)/src/apps/couch_mrview/test/*.t
	prove -v $(BASE_DIR)/src/apps/couch_replicator/test/*.t

testjs: testbuild
	$(ESCRIPT) $(BASE_DIR)/src/test/javascript/test_js.escript

testbuild: testclean
	$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/etap.erl
	$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/test_web.erl
	$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/test_util.erl
	$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/mustache.erl
	cc -DBSD_SOURCE $(COUCHDB_ETAP_DIR)/test_cfg_register.c \
		-o $(COUCHDB_ETAP_DIR)/test_cfg_register
	mkdir -p $(BASE_DIR)/src/test/out/data
	mkdir -p $(BASE_DIR)/src/test/out/bin
	mkdir -p $(BASE_DIR)/src/test/out/share
	mkdir -p $(BASE_DIR)/src/test/out/log
	cp $(BASE_DIR)/src/apps/couch/priv/couchjs $(BASE_DIR)/src/test/out/bin/
	cp -r $(BASE_DIR)/src/share/server $(BASE_DIR)/src/test/out/share
	cp -r $(BASE_DIR)/src/share/www $(BASE_DIR)/src/test/out/share
	cp $(BASE_DIR)/src/etc/couchdb/local.ini $(BASE_DIR)/src/test/out/

testclean:
	@rm -rf $(COUCHDB_ETAP_DIR)/*.beam
	@rm -rf $(BASE_DIR)/src/test/out
	@rm -rf $(COUCHDB_ETAP_DIR)/test_cfg_register
	@rm -rf $(COUCHDB_ETAP_DIR)/*.o

.PHONY: rebar
