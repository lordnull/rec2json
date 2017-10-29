%% Copyright 2012 Micah Warren
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(r2j_compile).

-export([simplify_fields/1]).
-export([type_declaration/2, export_type_declaration/2, export_declaration/2, export_declaration/3, additional_funcs/3]).

-record(record_field, {
    name, name_form,
    default_form,
    typelist = [],
    allow_any = true
}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(log(Msg, Args), ?debugFmt(Msg, Args)).
-define(log(Msg), ?log(Msg, [])).
-else.
-define(log(Msg, Args), io:format("~s:~p " ++ Msg ++ "\n", [?MODULE, ?LINE | Args])).
-define(log(Msg), ?log(Msg, [])).
-endif.

simplify_fields({attribute, _Line, record, {_RecName, Fields}}) ->
    simplify_fields(Fields, []).

simplify_fields([], Acc) ->
    lists:reverse(Acc);

simplify_fields([{record_field, Line, NameForm} | Tail], Acc) ->
    Name = erl_parse:normalise(NameForm),
    FieldRec = #record_field{name = Name, name_form = NameForm,
        default_form = undefined_default(Line), typelist = [undefined] },
    simplify_fields(Tail, [FieldRec | Acc]);

simplify_fields([{record_field, _Line, NameForm, Default} | Tail], Acc) ->
    Name = erl_parse:normalise(NameForm),
    FieldRec = #record_field{name = Name, name_form = NameForm, default_form = Default},
    simplify_fields(Tail, [FieldRec | Acc]);

simplify_fields([{typed_record_field, {record_field, Line, NameForm}, Type} | Tail], Acc) ->
    Name = erl_parse:normalise(NameForm),
    {AnyNess, Types} = extract_types(Type),
    AllowAny = case AnyNess of
        any -> true;
        _ -> false
    end,
    FieldRec = #record_field{name = Name, name_form = NameForm,
        default_form = undefined_default(Line), allow_any = AllowAny,
        typelist = [undefined | Types]},
    simplify_fields(Tail, [FieldRec | Acc]);

simplify_fields([{typed_record_field, {record_field, _L1, NameForm, Default}, Type} | Tail], Acc) ->
    Name = erl_parse:normalise(NameForm),
    {AnyNess, Types} = extract_types(Type),
    AllowAny = case AnyNess of
        any ->
            true;
        _ ->
            false
    end,
    FieldRec = #record_field{name = Name, name_form = NameForm,
        default_form = Default, allow_any = AllowAny, typelist = Types},
    simplify_fields(Tail, [FieldRec | Acc]).

undefined_default(Line) ->
    erl_parse:abstract(undefined, Line).

extract_types({type, _L1, union, Types}) ->
    extract_types(Types, []);
extract_types(T) when is_tuple(T) ->
    extract_types([T], []);
extract_types(T) when is_list(T) ->
    extract_types(T, []).

extract_types([], [undefined]) ->
    {any, []};

extract_types([], []) ->
    {any, []};

extract_types([], Acc) ->
    case lists:member(any, Acc) of
        true ->
            Acc2 = lists:delete(any, Acc),
            {any, Acc2};
        false ->
            {specific, Acc}
    end;
extract_types([{type, _L1, union, Types} | Tail], Acc) ->
    {_Specificness, Acc2} = extract_types(Types, Acc),
    extract_types(Tail, Acc2);
extract_types([{type, _L1, list, ListTypes} | Tail], Acc) ->
    ListTypes2 = extract_types(ListTypes),
    Acc2 = [{list, ListTypes2} | Acc],
    extract_types(Tail, Acc2);
% when a rocord field has a type defined as list(#record{}), the resulting
% type is {type, L, list, [{type, L, record, [RecordName]}]}, instead of the
% usual {type, L, record, RecordName}.
extract_types([{type, _L1, record, [{atom, _L2, RecordName}]} | Tail], Acc) ->
    % no way to know if it's a rec2json compiled record, so we'll just go with
    % it. If it's not (no to/from json), there's a catch for undef in the
    % rec2json module.
    extract_types(Tail, [{record, RecordName} | Acc]);
extract_types([{type, _L1, Type, TypeArgs} | Tail], Acc) ->
    % most likely not a good idea
    case supported_type(Type, TypeArgs) of
        true ->
            Normalised = [erl_parse:normalise(TypeArg) || TypeArg <- TypeArgs],
            Acc2 = [{Type, Normalised} | Acc],
            extract_types(Tail, Acc2);
        false ->
            extract_types(Tail, Acc);
        MFA ->
            Acc2 = [MFA | Acc],
            extract_types(Tail, Acc2)
    end;
extract_types([{remote_type, _L1, [{atom,_L2,Module},{atom,_L3,Function},Args]} | Tail], Acc) ->
    try [erl_parse:normalise(Abstract) || Abstract <- Args] of
        Normals ->
            extract_types(Tail, [{Module, Function, Normals} | Acc])
    catch
        _:_ ->
            extract_types(Tail, Acc)
    end;
extract_types([{user_type, _, _, _} | Tail], Acc) ->
    extract_types(Tail, Acc);
extract_types([Type | Tail], Acc) ->
    Acc2 = [erl_parse:normalise(Type) | Acc],
    extract_types(Tail, Acc2).

supported_type(Type, []) ->
    Supported = [integer, pos_integer, non_neg_integer, neg_integer, float,
        number, boolean, binary],
    case lists:member(Type, Supported) of
        true ->
            {r2j_type, Type, []};
        false ->
            false
    end;

supported_type(record, _) ->
    % no go way to know if the record is rec2json compiled or not; just have to
    % trust the user.
    true;

supported_type(_,_) ->
    false.

type_declaration(RecordName, Params) ->
    case proplists:get_value(generate_type, Params, true) of
        false ->
            {ok, []};
        true ->
            PropertyName = proplists:get_value(type_name, Params, RecordName),
            Attr = {attribute, 1, type, {PropertyName, {type, 1, record, [{atom, 1, RecordName}]},[]}},
            {ok, [Attr]}
    end.

export_type_declaration(RecordName, Params) ->
    case proplists:get_value(generate_type, Params, true) of
        false ->
            {ok, []};
        true ->
            PropertyName = proplists:get_value(type_name, Params, RecordName),
            Attr = {attribute, 1, export_type, [{PropertyName, 0}]},
            {ok, [Attr]}
    end.

additional_funcs(RecordName, Fields, Params) ->
    GenerateAccessors = proplists:get_value(generate_accessors, Params, true),
    AccessorFuncs = case GenerateAccessors of
        true -> getter_funcs(Fields);
        false -> []
    end,
    GenerateSetters = proplists:get_value(generate_setters, Params, true),
    SetterFuncs = case GenerateSetters of
        true -> setter_funcs(Fields);
        false -> []
    end,
    GenerateProperty = proplists:get_value(generate_type, Params, true),
    Converter = case GenerateProperty of
        true ->
            ProperyName = proplists:get_value(type_name, Params, RecordName),
            [converter_func(RecordName, ProperyName, length(Fields))];
        false -> []
    end,
    {ok, FieldListFunc} = get_field_names_func(Fields),
    {ok, TypeListFunc} = get_field_types_func(Fields),
    {ok, ToJsonA1} = to_json_arity1_func(),
    {ok, ToJsonA2} = to_json_arity2_func(),
    {ok, FromJsonA1} = from_json_arity1_func(RecordName, Fields),
    {ok, FromJsonA2} = from_json_arity2_func(RecordName, Fields),
    {ok, FromJsonA3} = from_json_arity3_func(),
    GrandFuncList = []
        ++ AccessorFuncs
        ++ SetterFuncs
        ++ [FieldListFunc]
        ++ [TypeListFunc]
        ++ Converter
        ++ [ToJsonA1]
        ++ [ToJsonA2]
        ++ [FromJsonA1]
        ++ [FromJsonA2]
        ++ [FromJsonA3]
    , {ok, GrandFuncList}.

export_declaration(RecordName, Fields) ->
    export_declaration(RecordName, Fields, []).

export_declaration(RecordName, Fields, Params) ->
    Line = proplists:get_value(line, Params, 1),
    GenerateAccessors = proplists:get_value(generate_accessors, Params, true),
    FieldDecs = case GenerateAccessors of
        true -> lists:map(fun getter_export_declaration/1, Fields);
        false -> []
    end,
    GenerateSetters = proplists:get_value(generate_setters, Params, true),
    SetFieldDecs = case GenerateSetters of
        true -> lists:map(fun setter_export_declaration/1, Fields);
        false -> []
    end,
    GenerateProperty = proplists:get_value(generate_type, Params, true),
    Converter = case GenerateProperty of
        true ->
            PropertyName = proplists:get_value(type_name, Params, RecordName),
            [{PropertyName, 1}];
        false -> []
    end,
    Decs = [{field_names, 0}, {field_types, 0}, {to_json, 1}, {to_json, 2},
        {from_json, 1}, {from_json, 2}, {from_json, 3}] ++ Converter ++ FieldDecs ++
        SetFieldDecs,
    {ok, {attribute, Line, export, Decs}}.

getter_export_declaration(Rec) ->
    {Rec#record_field.name, 1}.

setter_export_declaration(Rec) ->
    {Rec#record_field.name, 2}.

getter_funcs(Fields) ->
    EndSeq = 2 + length(Fields) - 1,
    Ns = lists:seq(2, EndSeq),
    Zipped = lists:zip(Ns, Fields),
    lists:map(fun getter_func/1, Zipped).

converter_func(RecordName, PropertyName, NumFields) ->
    {function,?LINE,PropertyName,1, [
        {clause,?LINE, [{var,?LINE,'Input'}], [], [
            {'case',?LINE, {call,?LINE, {remote,?LINE,{atom,?LINE,rec2json},{atom,?LINE,is_json_object}}, [{var,?LINE,'Input'}]}, [
                {clause,?LINE, [{atom,?LINE,true}], [], [
                    {call,?LINE,{atom,?LINE,from_json},[{var,?LINE,'Input'}]}
                ]},
                {clause,?LINE, [{atom,?LINE,false}], [], [
                    {'case',?LINE, {call,?LINE, {remote,?LINE,{atom,?LINE,rec2json},{atom,?LINE,is_record}}, [{var,?LINE,'Input'}, {atom,?LINE,RecordName}, {integer,?LINE,NumFields + 1}]}, [
                        {clause,?LINE, [{atom,?LINE,true}], [], [
                            {tuple, ?LINE, [{atom, ?LINE, ok}, {call,?LINE,{atom,?LINE,to_json},[{var,?LINE,'Input'}]}]}
                        ]},
                        {clause,?LINE, [{atom,?LINE,false}], [], [
                            {atom,?LINE,error}
                        ]}
                    ]}
                ]}
            ]}
        ]}
    ]}.

getter_func({ElementN, FieldRec}) ->
    {function, ?LINE, FieldRec#record_field.name, 1, [
        {clause, ?LINE, [{var, ?LINE, 'Record'}], [], [
            {call, ?LINE, {atom, ?LINE, element}, [
                {integer, ?LINE, ElementN},
                {var, ?LINE, 'Record'}
            ]}
        ]}
    ]}.

setter_funcs(Fields) ->
    EndSeq = 2 + length(Fields) - 1,
    Ns = lists:seq(2, EndSeq),
    Zipped = lists:zip(Ns, Fields),
    lists:map(fun setter_func/1, Zipped).

setter_func({ElementN, FieldRec}) ->
    {function, ?LINE, FieldRec#record_field.name, 2, [
        {clause, ?LINE, [{var, ?LINE, 'NewVal'}, {var, ?LINE, 'Record'}], [], [
            {call, ?LINE, {atom, ?LINE, setelement}, [{integer, ?LINE, ElementN}, {var, ?LINE, 'Record'}, {var, ?LINE, 'NewVal'}]}
        ]}
    ]}.

get_field_names_func(Fields) ->
    NameCons = lists:foldl(fun(FieldRec, ConsAcc) ->
        {cons, ?LINE, FieldRec#record_field.name_form, ConsAcc}
    end, {nil, ?LINE}, lists:reverse(Fields)),
    {ok, {function, ?LINE, field_names, 0, [
        {clause, ?LINE, [], [], [
            NameCons
        ]}
    ]}}.

get_field_types_func(Fields) ->
    TypeProps = lists:map(fun(FieldRecord) ->
        TypeProp = exportable_types(FieldRecord#record_field.typelist),
        Anyness = case FieldRecord#record_field.allow_any of
            true -> any;
            false -> specific
        end,
        {FieldRecord#record_field.name, {Anyness, TypeProp}}
    end, Fields),
    AbstractedProps = erl_parse:abstract(TypeProps),
    {ok, {function, ?LINE, field_types, 0, [
        {clause, ?LINE, [], [], [
            AbstractedProps
        ]}
    ]}}.

exportable_types({list, ListDef}) ->
    Exportable = exportable_types(ListDef),
    {list, Exportable};

exportable_types({AtomType, []}) when is_atom(AtomType) ->
    AtomType;

exportable_types({record, [RecName]}) ->
    {record, RecName};

exportable_types(List) when is_list(List) ->
    NoUndef = lists:delete(undefined, List),
    % the lists reverse is at the end because the earlier simplify fields
    % reverses the order the types were declared. By reversing it here,
    % the types will show up in the same ordered they ere defined for the
    % record.
    Cleansed = lists:map(fun exportable_types/1, lists:reverse(NoUndef)),
    case NoUndef of
        List ->
            Cleansed;
        _Shorter ->
            [undefined | Cleansed]
    end;

exportable_types(Wut) ->
    Wut.

to_json_arity1_func() ->
    {ok, {function, ?LINE, to_json, 1, [
        {clause, ?LINE, [{var, ?LINE, 'Struct'}], [], [
            {call, ?LINE, {remote, ?LINE, {atom, ?LINE, rec2json}, {atom, ?LINE, to_json}}, [{var, ?LINE, 'Struct'}, {nil, ?LINE}]}
        ]}
    ]}}.

to_json_arity2_func() ->
    {ok, {function, ?LINE, to_json, 2, [
        {clause, ?LINE, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Options'}], [[{call, ?LINE, {atom, ?LINE, is_tuple}, [{var, ?LINE, 'Struct'}]}]], [
            {call, ?LINE, {remote, ?LINE, {atom, ?LINE, rec2json}, {atom, ?LINE, to_json}}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Options'}]}
        ]},
        {clause, ?LINE, [{var, ?LINE, 'Options'}, {var, ?LINE, 'Struct'}], [[{call, ?LINE, {atom, ?LINE, is_tuple}, [{var, ?LINE, 'Struct'}]}]], [
            {call, ?LINE, {atom, ?LINE, to_json}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Options'}]}
        ]}
    ]}}.

blank_record(RecName, Fields) ->
    ValueListSansName = lists:map(fun(FieldRec) ->
        FieldRec#record_field.default_form
    end, Fields),
    ElementList = [{atom, ?LINE, RecName} | ValueListSansName],
    {tuple, ?LINE, ElementList}.

from_json_arity1_func(RecName, Fields) ->
    BlankTuple = blank_record(RecName, Fields),
    {ok, {function, ?LINE, from_json, 1, [
        {clause, ?LINE, [{var, ?LINE, 'Json'}], [], [
            {call, ?LINE, {remote, ?LINE, {atom, ?LINE, rec2json}, {atom, ?LINE, from_json}}, [BlankTuple, {var, ?LINE, 'Json'}, {nil, ?LINE}]}
        ]}
    ]}}.

from_json_arity2_func(RecName, Fields) ->
    BlankRec = blank_record(RecName, Fields),
    {ok,{function, ?LINE,from_json,2, [
        from_json_arity2_no_seed_json_first(BlankRec),
        from_json_arity2_no_seed_json_second(BlankRec),
        from_json_arity2_seed_first(),
        from_json_arity2_seed_second()
    ]}}.

from_json_arity2_no_seed_json_first(BlankRec) ->
    Args = [{var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}],
    Guards = [[
        {call, ?LINE, {atom, ?LINE, is_map}, [{var, ?LINE, 'Json'}]},
        {call, ?LINE, {atom, ?LINE, is_list}, [{var, ?LINE, 'Opts'}]}
    ]],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [BlankRec, {var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.

from_json_arity2_no_seed_json_second(BlankRec) ->
    Args = [{var, ?LINE, 'Opts'}, {var, ?LINE, 'Json'}],
    Guards = [[
        {call, ?LINE, {atom, ?LINE, is_map}, [{var, ?LINE, 'Json'}]},
        {call, ?LINE, {atom, ?LINE, is_list}, [{var, ?LINE, 'Opts'}]}
    ]],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [BlankRec, {var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.

from_json_arity2_seed_first() ->
    Args = [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}],
    Guards = [[
        {call, ?LINE, {atom, ?LINE, is_tuple}, [{var, ?LINE, 'Struct'}]},
        {call, ?LINE, {atom, ?LINE, is_map}, [{var, ?LINE, 'Json'}]}
    ]],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}, {nil, ?LINE}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.

from_json_arity2_seed_second() ->
    Args = [{var, ?LINE, 'Json'}, {var, ?LINE, 'Struct'}],
    Guards = [[
        {call, ?LINE, {atom, ?LINE, is_tuple}, [{var, ?LINE, 'Struct'}]},
        {call, ?LINE, {atom, ?LINE, is_map}, [{var, ?LINE, 'Json'}]}
    ]],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}, {nil, ?LINE}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.

from_json_arity3_func() ->
    Gaurds = [[
        {call, ?LINE, {atom, ?LINE, is_tuple}, [{var, ?LINE, 'Struct'}]},
        {call, ?LINE, {atom, ?LINE, is_map}, [{var, ?LINE, 'Json'}]},
        {call, ?LINE, {atom, ?LINE, is_list}, [{var, ?LINE, 'Opts'}]}
    ]],
    RemoteCall = {call, ?LINE, {remote, ?LINE, {atom, ?LINE, rec2json},{atom,?LINE,from_json}}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}]},
    ArgOrders = [
        ['Struct', 'Json', 'Opts'],
        ['Struct', 'Opts', 'Json'],
        ['Json', 'Struct', 'Opts'],
        ['Json', 'Opts', 'Struct'],
        ['Opts', 'Json', 'Struct'],
        ['Opts', 'Struct', 'Json']
    ],
    Clauses = lists:map(fun(Args) ->
        Vars = [{var, ?LINE, A} || A <- Args],
        {clause, ?LINE, Vars, Gaurds, [RemoteCall]}
    end, ArgOrders),
    {ok, {function, ?LINE, from_json, 3, Clauses}}.
