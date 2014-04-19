BASE_DIR=$(CURDIR)
SUPPORT_DIR=$(BASE_DIR)/support
OVERLAY_VARS ?=
PACKAGE_NAME=apache-couchdb
RELDIR=$(BASE_DIR)/rel/$(PACKAGE_NAME)

ifeq ($(OS),Windows_NT)
	ERLC ?= "$(shell where erlc)"
	ESCRIPT ?= "$(shell where escript)"
	PYTHON ?= "$(shell where python)"
	CP=python $(SUPPORT_DIR)/cp.py
	RM=python $(SUPPORT_DIR)/rm.py
	MKDIR=python $(SUPPORT_DIR)/mkdir.py
	CC=cl.exe
	COUTFLAG ?= /Fe
	CDEFINE ?= /D
else
	ERLC ?= "$(shell which erlc)"
	ESCRIPT ?= "$(shell which escript)"
	PYTHON ?= "$(shell which python)"
	CP=cp -r
	RM=rm -rf
	MKDIR=mkdir -p
	CC ?= cc
	COUTFLAG ?= -o
	CDEFINE ?= -D
endif

$(if $(ERLC),,$(warning "Warning: No Erlang found in your path, this will probably not work"))

$(if $(ESCRIPT),,$(warning "Warning: No escript found in your path, this will probably not work"))

$(if $(PYTHON),,$(warning "Warning: No python found in your path, this may not work, for Windows or for documentation builds"))

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
	@$(BASE_DIR)/rebar -q compile

deps: rebar
	@$(BASE_DIR)/rebar -q get-deps

clean: docclean
	@$(BASE_DIR)/rebar -q clean

distclean: clean rebarclean relclean

generate:
	@$(BASE_DIR)/rebar -q generate $(OVERLAY_VARS)

rel: generate

relclean: reldocclean
	@$(RM) rel/apache-couchdb

check: test testjs

#
# rebar
#

rebar: $(BASE_DIR)/rebar

$(BASE_DIR)/rebar:
	@echo "==> build rebar"
	@(cd $(BASE_DIR)/support/rebar && $(ESCRIPT) bootstrap)
	@$(CP) $(BASE_DIR)/support/rebar/rebar $(BASE_DIR)/rebar
	@$(CP) $(BASE_DIR)/support/rebar/rebar.cmd $(BASE_DIR)/rebar.cmd

rebarclean:
	@(cd $(BASE_DIR)/support/rebar && \
		$(RM) rebar ebin/*.beam inttest/rt.work rt.work .test)
	@$(RM) $(BASE_DIR)/rebar

#
# DOCS
#

DOC_SRCDIR=$(BASE_DIR)/share/doc/src
DOC_BUILDDIR=$(BASE_DIR)/share/doc/build
DOC_RELDIR=$(RELDIR)/share/doc
SPHINXOPTS = -n -c $(DOC_SRCDIR) \
			 -A local=1 \
			 $(DOC_SRCDIR)

reldoc: reldocclean doc
	$(MKDIR) $(DOC_RELDIR)
	$(CP) $(DOC_BUILDDIR)/html $(DOC_RELDIR)
	$(CP) $(DOC_BUILDDIR)/latex/CouchDB.pdf $(DOC_RELDIR)
	$(CP) $(DOC_BUILDDIR)/texinfo/CouchDB.info $(DOC_RELDIR)

doc: html pdf texinfo

html:
	@$(MKDIR) $(DOC_BUILDDIR)
	$(SUPPORT_DIR)/doc/sphinx-build \
		-b html $(SPHINXOPTS) $(DOC_BUILDDIR)/html

pdf:
	@$(MKDIR) $(DOC_BUILDDIR)
	$(SUPPORT_DIR)/doc/sphinx-build \
		-b latex $(SPHINXOPTS) $(DOC_BUILDDIR)/latex
	$(MAKE) -C $(DOC_BUILDDIR)/latex all-pdf

texinfo:
	@$(MKDIR) $(DOC_BUILDDIR)
	$(SUPPORT_DIR)/doc/sphinx-build \
		-b texinfo $(SPHINXOPTS) $(DOC_BUILDDIR)/texinfo
	$(MAKE) -C $(DOC_BUILDDIR)/texinfo info

docclean:
	$(RM) $(DOC_BUILDDIR)

reldocclean:
	$(RM) $(DOC_RELDIR)

#
# TESTS
#
COUCHDB_ETAP_DIR=$(BASE_DIR)/test/etap
export COUCHDB_ETAP_DIR


ERL_FLAGS=-pa $(BASE_DIR)/src/*/ebin -pa $(COUCHDB_ETAP_DIR)
export ERL_FLAGS

test: testbuild
	@echo "==> test couch_collate"
	@cd $(BASE_DIR)/src/couch_collate && \
		prove $(BASE_DIR)/src/couch_collate/t/*.t
	@echo "==> test couch core"
	@prove $(COUCHDB_ETAP_DIR)/*.t
	@echo "==> test couch_mrview"
	@prove $(BASE_DIR)/src/couch_mrview/test/*.t
	@echo "==> test couch_replicator"
	@prove $(BASE_DIR)/src/couch_replicator/test/*.t

verbose-test: testbuild
	@echo "==> test couch_collate"
	@cd $(BASE_DIR)/src/couch_collate && \
		prove -v $(BASE_DIR)/src/couch_collate/t/*.t
	@echo "==> test couch core"
	@prove -v $(COUCHDB_ETAP_DIR)/*.t
	@echo "==> test couch_mrview"
	@prove -v $(BASE_DIR)/src/couch_mrview/test/*.t
	@echo "==> test couch_replicator"
	@prove -v $(BASE_DIR)/src/couch_replicator/test/*.t

testjs: testbuild
	@$(ESCRIPT) $(BASE_DIR)/test/javascript/test_js.escript

testbuild: testclean
	@echo "==> init test environment"
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/etap.erl
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/test_web.erl
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/test_util.erl
	@$(ERLC) -v -o $(COUCHDB_ETAP_DIR) $(COUCHDB_ETAP_DIR)/mustache.erl
	@$(CC) $(CDEFINE)BSD_SOURCE $(COUCHDB_ETAP_DIR)/test_cfg_register.c \
		$(COUTFLAG)$(COUCHDB_ETAP_DIR)/test_cfg_register
	@$(MKDIR) $(BASE_DIR)/test/out/data
	@$(MKDIR) $(BASE_DIR)/test/out/bin
	@$(MKDIR) $(BASE_DIR)/test/out/share
	@$(MKDIR) $(BASE_DIR)/test/out/log
	@$(CP) $(BASE_DIR)/src/couch/priv/couchjs $(BASE_DIR)/test/out/bin/
	@$(CP) $(BASE_DIR)/share/server $(BASE_DIR)/test/out/share
	@$(CP) $(BASE_DIR)/share/www $(BASE_DIR)/test/out/share
	@$(CP) $(BASE_DIR)/etc/couchdb/local.ini $(BASE_DIR)/test/out/

testclean:
	@$(RM) $(COUCHDB_ETAP_DIR)/*.beam
	@$(RM) $(BASE_DIR)/test/out
	@$(RM) $(COUCHDB_ETAP_DIR)/test_cfg_register
	@$(RM) $(COUCHDB_ETAP_DIR)/*.o

.PHONY: rebar
