REPO        ?= rec2json

.PHONY: rel deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean: testclean
	./rebar clean

distclean: clean
	./rebar delete-deps

TEST_LOG_FILE := eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

DIALYZER_APPS = kernel stdlib syntax_tools compiler

include tools.mk

test_all: test dialyzer