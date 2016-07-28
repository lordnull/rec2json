PROJECT = rec2json
PROJECT_DESCRIPTION = Compile erlang record definitions into modules to convert them to/from json easily.
PROJECT_VERSION = 3.2.1

TEST_DEPS = proper jsx
dep_proper = git https://github.com/manopapad/proper master
dep_jsx = git https://github.com/talentdeficit/jsx v2.4.0

include erlang.mk
