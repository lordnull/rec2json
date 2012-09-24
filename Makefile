REPO        ?= rec2json

.PHONY: rel deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean: testclean
	./rebar clean

distclean: clean devclean relclean ballclean
	./rebar delete-deps


TEST_LOG_FILE := eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

ct: clean deps compile
	./rebar ct skip_deps=true

eunit: clean deps compile
	./rebar eunit skip_deps=true

# Test each dependency individually in its own VM
test: deps compile testclean
	./rebar eunit skip_deps=true
	./rebar ct skip_deps=true
