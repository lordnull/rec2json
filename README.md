# rec2json
[![Build Status](https://travis-ci.org/lordnull/rec2json.png)](https://travis-ci.org/lordnull/rec2json)

## Overview

Rec2json takes standard erlang source or include files, scans them for
record definitions, and generates a module for each record to convert that
record to and from the
[proposed erlang json standard](http://www.erlang.org/eeps/eep-0018.html)

Alternatively, rec2json includes a parse_transform that allows a module which
defines (or includes the definition of) a record of the same name to export
the json conversion and record field accessor functions.

## Features

* Resulting modules can be used as parameterized modules or pure erlang.
* Includes a parse transform
* Resulting escript can create modules from record definition only.
* Limited type checking on json -> record conversion.
* Atom 'undefined' fields in records optionally skipped or set to null.
* Atom 'null' in json optionally converted to 'undefined'.
* Post processing options on record -> json convertion.
* Seed json -> record conversion with a record.
* Nested json -> record and record -> json conversions for other
records that have been compiled using rec2json.
* Generated module has accessor functions for fields and field_names for
list of fields in the record.

## Compiling

    make

To run tests:

    make eunit

    make test

## Parse_transform

Rec2json can apply a parse_transform to a module that includes a record of the
same name to add the to_json/N and from_json/N functions, as well as the field
accessors. Simpley add the following line near the module attribute:

    -compile([{parse_transform, rec2json}]).

When using only the parse transform, you do not need to have the rec2json script
available. You will still need rec2json in the release for your application.

## Compiling Records

Fully implemented is the scan_file and scan_string functions.

    rec2json_compile:scan_file("src_file.hrl", []).
    rec2json_compile:scan_string("-record(a, {b = 1 :: integer()}).", []).
    rec2json_compile:scan_string("-record(a, {b :: #b{}}). \n -record(b, {c = 1 :: integer()}).").

The Options (empty list at the end) are:

* {imports_dir, "path/to/imports"}
* {output_dir, "path/to/ebin"}

Multple files can be scanned by either using wildcards or listing them:

    rec2json_compile:scan_files("include/*.hrl", Options).
    rec2json_compile:scan_files(["rec.hrl", "other_rec.hrl"], Options).

To generate a stand-alone script for compiling:

    make script

This will create a "rec2json" script that can be run:

    ./rec2json -src=include/*.hrl -dest=ebin -include=include

## Use

The records are unchanged and can be used normally. The module to convert
the record to and from json has the same name as the record. A record can
also be used as a paramterized module, making it simple to use with the
[erlydtl Django templates](https://github.com/evanmiller/erlydtl) project.

The given examples use the following record and record defintion:

```erlang
-record(person {
    name :: binary(),
    age = 0 :: pos_integer(),
    spouse :: #person{}
}).
Record = #person{ name = <<"John">>, age = 32, spouse = undefined }.
```

### to_json

To convert a record to a json structure:

```erlang
Json = Record:to_json().
Json = person:to_json(Record).
[{name, <<"John">>}, {age, 32}] = Json.
```

The to_json function can take a list of mutators. Mutators are applied in
the order listed except {null_is_undefined}. Supported mutators are:

* Turn undefined into null instead of skipping the field

```erlang
Record:to_json([{null_is_undefined}]).
person:to_json(Record, [{null_is_undefined}]).
```

* Add a property

```erlang
Record:to_json([{single, true}]).
person:to_json(Record, [{single, true}]).
```

* Remove a property

```erlang
Record:to_json([age]).
person:to_json(Record, [age]).
```

* Modify based only on the json

```erlang
ModFunc = fun(Json) ->
    case proplists:get_value(spouse, Json) of
        undefined ->
            [{single, true} | Json];
        _ ->
            [{single, false} | Json]
    end
end.
Record:to_json([ModFunc]).
person:to_json(Record, [ModFunc]).
```

* Modify based on both json and record

```erlang
ModFunc = fun(Json, Record) ->
    case Record#person.spouse of
        undefined ->
            [{single, true} | Json];
        _ ->
            [{single, false} | Json]
    end
end.
Record:to_json([ModFunc]).
person:to_json(Record, [ModFunc]).
```

### from_json

Converting from a json structure to a record is just as simple:

```erlang
{ok, Record} = person:from_json([
    {<<"name">>, <<"John">>},
    {<<"age">>, 32},
    {<<"spouse">>, null}
]).
```

It may be desireable to change 'null' into 'undefined' in the record:

```erlang
{ok, Record} = person:from_json(Json, [null_is_undefined]).
```

It may be desireable to start with an existing record instead of creating
a new one:

```erlang
{ok, Record2} = Record:from_json(Json).
{ok, Record2} = person:from_json(Json, Record).
{ok, Record2} = person:from_json(Record, Json).
{ok, Record2} = Record:from_json(Json, [null_is_undefined]).
{ok, Record2} = person:from_json(Record, Json, [null_is_undefined]).
```

If the json structure has a type that connot be reconciled with a type
specified by the record definition, a list of fields with possible errors
is returned. The record will have the data that was in the json structure.
An untyped record field is the same as having the type 'any()'. There are
no warings about missing properties in the json, they simply retain the
default value of the record.

```erlang
{ok, Record, [age]} = person:from_json([{<<"age">>, <<"32">>}]).
```

### Including in a project

If all you are using is the parse_transform, simply add rec2json as a
required application.

To be able to create modules from records without the
parse transform, you will need to add the rec2json script to your path in some
manner. Add a call to the rec2json script during your build (in your Makefile
or rebar.config precompile hook).

## Type Checking and Converstion

Type conversion attempts to be as transparent and intuitive as possible. There
are some types that json does not represent directly, and some types that have
additional checking implemented.

Record fields that have atoms as types will have binary values in the json
checked.  If the atom converted to the binary is equal to the json value, the
atom value is put into the record. When converting a record to json, atom
values will be converted to binaries to conform to the erlang spec.

Lists have their types checked. If there is an invalid type, the invalid type
is placed in the list, but the warning message has the index of the invalid type
placed in the warning path list. For example:

```erlang
-record(list_holder, {
    ids :: [integer()]
}).

type_mismatch() ->
    Json = [{ids, [<<"invalid">>, 3]}],
    {ok, Record, Warnings} = list_holder:from_json(Json),
    #list_holder{ids = [<<"invalid">>, 3]} = Record,
    [[ids, 1]] = Warnings.
```

Proplists will match the first record type. A warning is emitted if that
record was not compiled using rec2json (eg: RecordName:from_json/2 is not
an exported function). If the compilation emits warnings, the resulting warning
list has the field name prepended to each.  For example:

```erlang
-record(outer, {
    in_field :: #inner{}
}).
-record(inner, {
    count :: integer()
}).

type_mismatch() ->
    Json = [{in_field, [{count, <<"0">>}]}],
    {ok, Record, Warnings} = outer:from_json(Json),
    #outer{in_field = #inner{ count = <<"0">> } } = Record,
    [[in_filed, count]] = Warnings.
```

If a record field is not typed, or has the type "any()", no warning is ever
emitted for that field.

User defined types are not checked. During record compile, any unrecognized
types are skipped. The result is any record field with only user defined types
is treated as having the type 'any()' when converting from json.

Currently defined types checked:

* integer()
* pos_integer()
* non_neg_integer()
* neg_integer()
* float()
* number()
* boolean()
* binary()
* [supported_type()]
* #record{} when record has record:from_json/2 exported
* atom (note it is not atom())
* null when converting to undefined or back

## Contributing

Fork and submit a pull request with relevent tests.
