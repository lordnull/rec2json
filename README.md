# rec2json
[![Build Status](https://travis-ci.org/lordnull/rec2json.png?branch=master)](https://travis-ci.org/lordnull/rec2json)

## Overview

Rec2json is a parse transform that takes a module which defines a record of
the same name and adds to_json, from_json, and introspection functions. The
to_json and from_json convert to and from the map based format used by jsx and
other json encoding and decoding libraries.

## Features

* Uses a parse transform.
* Type checking on json -> record conversion using primitive built-in types.
* Type checking can be extended to include user defined types.
* Atom 'undefined' fields in records optionally skipped or set to null.
* Atom 'null' in json optionally converted to 'undefined'.
* Post processing options on record -> json convertion.
* Seed json -> record conversion with a record.
* Nested json -> record and record -> json conversions for other records
that have been compiled using rec2json.
* Generated module has accessor functions for fields and field_names for
list of fields in the record.
* Above feature can be surpressed, and is careful by default.
* Generated module exports functions to examine structure and types for a
record.

## Compiling

    make

To run tests:

    make tests

    make check

## Use

The rec2json parse_transform looks for a record with the same name as the
module. It doesn't matter whether the record was defined directly in the
module or in an include file.

    -compile([{parse_transform, rec2json}]).

The transformed modules depend on the rec2json application's modules.

The records are unchanged and can be used normally.

Options are passed to the rec2json parse transform through compile options.
The parse transform checks for the key 'rec2json' in the compile options.
The value is expected to be a proplist.

Options can also be passed in on a per-module basis by adding one or more
`rec2json` module attributes. A `rec2json` module attribute can either be a
tuple for one option, or a list of option tuples. They all get mashed together.
Using the same option more than once has undefined behavior.

Options are:

<table>
  <tr>
    <th>Option</th> <th>Default: Values</th> <th>Description</th>
  </tr>
  <tr>
    <td>generate_accessors</td> <td>true : boolean()</td> <td>If set to
true, functions for accessing the fields of a record are exported and
created. If set to false, they are not created nor exported.</td>
  </tr>
	<tr>
		<td>generate_setters</td> <td>true : boolean()</td> <td> If set to
true, functions for setting the fields of a record are created and
exported. These are of the form Field(NewVal, Record). If set to false, they
are not created nor exported.
		</td>
	</tr>
	<tr>
		<td>careful</td> <td>true : boolean()</td> <td> If set to true,
rec2json's parse transform avoids altering or adding functions that are
already defined in the module. This means you can override the default
to_json/1 function to call to_json/2 with a specific set of options.</td>
	</tr>
    <tr>
        <td>generate_type</td> <td>true : boolean()</td> <td> If set to
true, a type is generated for the record, and that type is exported. In
addition, a function is generated so that other rec2json records using the
exported type work as expected.</td>
    </tr>
    <tr>
        <td>type_name</td> <td> ?MODULE : atom()</td> <td> If generate_type is
true, this changes the type name and the function name for the conversion
function.</td>
    </tr>
</table>

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
Json = person:to_json(Record).
#{name := <<"John">>, age := 32} = Json.
```

The to_json function can take a list of mutators. Mutators are applied in
the order listed except {null_is_undefined}. Supported mutators are:

* Turn undefined into null instead of skipping the field

```erlang
person:to_json(Record, [{null_is_undefined}]).
```

* Add a property

```erlang
person:to_json(Record, [{single, true}]).
person:to_json(Record, [#{single => true}).
person:to_json(Record, [#{single => true, employed => true}).
```

* Remove a property

```erlang
person:to_json(Record, [age]).
```

* Modify based only on the json

```erlang
ModFunc = fun(Json) ->
    case maps:find(spouse, Json) of
        error ->
            Json#{single => true}
        _ ->
            Json#{single => false}
    end
end.
person:to_json(Record, [ModFunc]).
```

* Modify based on both json and record

```erlang
ModFunc = fun(Json, Record) ->
    case Record#person.spouse of
        undefined ->
            Json#{single => true};
        _ ->
            Json#{single => false}
    end
end.
person:to_json(Record, [ModFunc]).
```

### from_json

Converting from a json structure to a record is just as simple:

```erlang
{ok, Record} = person:from_json(#{
    <<"name">> => <<"John">>,
    <<"age">> => 32,
    <<"spouse">> => null
}).
```

It may be desirable to change 'null' into 'undefined' in the record:

```erlang
{ok, Record} = person:from_json(Json, [null_is_undefined]).
```

It may be desirable to start with an existing record instead of creating
a new one:

```erlang
{ok, Record2} = person:from_json(Json, Record).
{ok, Record2} = person:from_json(Record, Json).
{ok, Record2} = person:from_json(Record, Json, [null_is_undefined]).
```

If the json structure has a type that cannot be reconciled with a type
specified by the record definition, a list of fields with possible errors
is returned. The record will have the data that was in the json structure.
An untyped record field is the same as having the type 'any()'. There are
no warnings about missing properties in the json, they simply retain the
default value of the record.

```erlang
{ok, Record, [age]} = person:from_json(#{<<"age">> => <<"32">>}).
```

### Including in a project

If all you are using is the parse_transform, simply add rec2json as a
required application.

## Type Checking and Conversion

Type conversion attempts to be as transparent and intuitive as possible.
There are some types that json does not represent directly, and some types
that have additional checking implemented.

Record fields that have atoms as types will have binary values in the json
checked. If the atom converted to the binary is equal to the json value, the
atom value is put into the record. When converting a record to json, atom
values will be converted to binaries.

Lists have their types checked. If there is an invalid type, the invalid
type is placed in the list, but the warning message has the index of the
invalid type placed in the warning path list. For example:

```erlang
-record(list_holder, {
    ids :: [integer()]
}).

type_mismatch() ->
    Json = #{ids => [<<"invalid">>, 3]},
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
    Json = #{in_field => #{count => <<"0">>}},
    {ok, Record, Warnings} = outer:from_json(Json),
    #outer{in_field = #inner{ count = <<"0">> } } = Record,
    [[in_filed, count]] = Warnings.
```

If a record field is not typed, or has the type "any()", no warning is ever
emitted for that field.

Type checking comes in two flavors: built-in, and user defined.

### Built-in types

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
* atom (note it is not atom())*
* null when converting to undefined or back

The type `atom()` is not supported because a primary use case for rec2json
is to convert untrusted data, such as an http post request. Converting
untrusted data into atoms can exhaust the erlang vm's atom table, or worse
exhaust the machine's memory. Rec2json is, by default, safe.

It is still possible to apply a type to a record field so that json strings
will be converted to the equivalent atom using either a user defined type,
or the provided `r2j_type:unsafe_atom()` type.

### User defined types

A user defined type is the same as an external type. When going to or from
json, rec2json will check to see if there is a translation function
matching the module and type of the type given. A translation function
should have an arity 1 greater than the type has parameters. The function
should either return `{ok, NewVal}` or `error`.

For example, given the module:

```erlang
-module(type_example).
-compile([{parse_transform, rec2json}]).

% These two lines exist to satisfy dialyzer.
-type point() :: {number(), number()}.
-export_type([point/0]).

-record(type_example, {
	some_field :: module:function(arg1, arg2),
	xy = {0, 0} :: type_example:point()
}).

-export([point/1]).

point({X,Y}) when is_number(X), is_number(Y) ->
	{ok, [X,Y]};

point([X, Y]) when is_number(X), is_number(Y) ->
	{ok, {X, Y}};

point(_) ->
	error.
```

When using to_json or from_json, rec2json will check to see if `module`
exports a function named `function` with arity 3. If it exists, rec2json
will call the function with the args listed in the type, and the current
value of the field (either from the record or from the json) prepended to
the arguments.

When used in `from_json`, returns `error`, and all other types have been
tested, `{ok, Rec, Warnings}` is returned. When used in `to_json`, an
error is thrown.

## Contributing

Fork and submit a pull request with relevant tests.
