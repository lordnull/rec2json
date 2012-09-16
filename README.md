# rec2json

## Overview

Rec2json takes standard erlang source or include files, scans them for
record definitions, and generates a module for each record to convert that
record to and from the
[proposed erlang json standard](http://www.erlang.org/eeps/eep-0018.html)

**This should be considered alpha software.  Many of the features listed
are not done**

## Features

Features that are not full implemented are marked with (WIP).

* Resulting modules can be used as parameterized modules or pure erlang.
* (WIP) Type checking on json -> record conversion.
* Atom 'undefined' fields in records optionally skipped or set to null.
* Atom 'null' in json optionally converted to 'undefined'.
* Post processing options on record -> json convertion.
* Seed json -> record conversion with a record.
* (WIP) Nested json -> record and record -> json conversions for other
records that have been compiled using rec2json.
* Resulting modules have no dependency on rec2json.

## Compiling

    ./rebar get-deps compile

To run tests:

    ./rebar eunit ct skip_deps=true

## Compiling Records

Full implemented is the scan_file and scan_string functions.  The scan file
processes the given file, while scan_string can only process a single record
definition given as a string.

    rec2json_compile:scan_file("src_file.hrl", []).
    rec2json_compile:scan_string("-record(a, {b = 1 :: integer()}).", []).

The Options (empty list at the end) are:

* {imports_dir, "path/to/imports"}
* {output_dir, "path/to/ebin"}

Multple files can be scanned by either using wildcards or listing them:

    rec2json_compile:scan_files("include/*.hrl", Options).
    rec2json_compile:scan_files(["rec.hrl", "other_rec.hrl"], Options).

To generate a stand-alone script for compiling:

    ./rebar escriptize skip_deps=true

This will create a "rec2json" script that can be run:

    ./rec2json -src=include/*.hrl -dest=ebin -include=include

## Use

The records are unchanged and can be used normally.  The module to convert
the record to and from json has the same name as the record.  A record can
also be used as a paramterized module, making is simple to use with the
[erlydtl Django templates](https://github.com/evanmiller/erlydtl) project.

The given examples use the following record and record defintion:

    -record(persion {
        name :: binary(),
        age = 0 :: pos_integer(),
        spouse :: #person{}
    }).
    Record = #person{ name = <<"John">>, age = 32, spouse = undefined }.

### to_json

To convert a record to a json structure:

    Json = Record:to_json().
    Json = person:to_json(Record).
		[{name, <<"John">>}, {age, 32}].

The to_json function can take a list of mutators.  Mutators are applied in
the order listed except {null_is_undefined}.  Supported mutators are:

* Turn undefined into null instead of skipping the field

    <pre>Record:to_json([{null_is_undefined}]).
    person:to_json(Record, [{null_is_undefined}]).</pre>

* Add a property

    <pre>Record:to_json([{single, true}]).
    person:to_json(Record, [{single, true}]).</pre>

* Remove a property

    <pre>Record:to_json([age]).
    person:to_json(Record, [age]).</pre>

* Modify based only on the json

    <pre>ModFunc = fun(Json) ->
        case proplists:get_value(spouse, Json) of
            undefined ->
                [{single, true} | Json];
            _ ->
                [{single, false} | Json]
        end
    end.
    Record:to_json([ModFunc]).
    person:to_json(Record, [ModFunc]).</pre>

* Modify based on both json and record

    <pre>ModFunc = fun(Json, Record) ->
        case Record#person.spouse of
            undefined ->
                [{single, true} | Json];
            _ ->
                [{single, false} | Json]
        end
    end.
    Record:to_json([ModFunc]).
    person:to_json(Record, [ModFunc]).</pre>

### from_json

Converting from a json structure to a record is just as simple:

    {ok, Record} = person:from_json([
        {<<"name">>, <<"John">>},
        {<<"age">>, 32},
        {<<"spouse">>, null}
    ]).

It may be desireable to change 'null' into 'undefined' in the record:

    {ok, Record} = person:from_json(Json, [null_is_undefined]).

It may be desireable to start with an existing record instead of creating
a new one:

    {ok, Record2} = Record:from_json(Json).
    {ok, Record2} = person:from_json(Json, Record).
    {ok, Record2} = Record:from_json(Json, [null_is_undefined]).
    {ok, Record2} = person:from_json(Json, Record, [null_is_undefined]).

If the json struction has a type that connot be reconciled with a type
specified by the record definition, a list of fields with possible errors
is returned.  The record will have the data that was in the json structure.
An untyped record field is the same as having the type 'any()'.  There are
no warings about missing properties in the json, they simply retain the
default value of the record.

    {ok, Record, [age]} = person:from_json([{<<"age">>, <<"32">>}]).

## Contributing

Fork and submit a pull request with relevent tests.
