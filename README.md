# rec2json

## Overview

Rec2json takes standard erlang source or include files, scans them for
record definitions, and generates a module for each record to convert that
record to and from the
[proposed erlang json standard](http://www.erlang.org/eeps/eep-0018.html)

## Features

Features that are not full implemented are marked with (WIP).

* Resulting modules can be used as parameterized modules or pure erlang.
* Limited type checking on json -> record conversion.
* Atom 'undefined' fields in records optionally skipped or set to null.
* Atom 'null' in json optionally converted to 'undefined'.
* Post processing options on record -> json convertion.
* Seed json -> record conversion with a record.
* Nested json -> record and record -> json conversions for other
records that have been compiled using rec2json.

## Compiling

    make

To run tests:

    make eunit

## Compiling Records

Full implemented is the scan_file and scan_string functions. The scan file
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
[{name, <<"John">>}, {age, 32}].
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

The easiest way to use rec2json in your project is to add rec2json to your
erlang libs path, and add the created rec2json script to your path.  Add
rec2json as a required application.  Finally, during your build (in your
Makefile or rebar.config precompile hook) call rec2json.

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

User defined types are not checked.

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
