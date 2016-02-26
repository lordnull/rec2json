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

-export([scan_file/2, scan_string/2]).
-export([simplify_fields/1, export_declaration/1, export_declaration/2, additional_funcs/3]).

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

scan_file(Hrl, Opts) ->
    Imports = proplists:get_value(imports_dir, Opts, []),
    {ok, Handle} = epp:open(Hrl, Imports, []),
    {ok, Forms} = read_epp_forms(Handle),
    Modules = analyze_forms(Forms, Opts),
    output(Modules, proplists:get_value(output_dir, Opts, ".")).

read_epp_forms(Handle) ->
    read_epp_forms(Handle, []).

read_epp_forms(Handle, Acc) ->
    case epp:parse_erl_form(Handle) of
        {ok, Form} ->
            read_epp_forms(Handle, [Form | Acc]);
        {eof, _L} ->
            {ok, lists:reverse(Acc)}
    end.

scan_string(Str, Opts) ->
    %Imports = proplists:get_value(imports_dir, Opts, []),
    {ok, Tokens, _Lines} = erl_scan:string(Str),
    LineTokens = split_tokens_by_dots(Tokens),
    Forms = parse_forms(LineTokens),
    Modules = analyze_forms(Forms, Opts),
    output(Modules, proplists:get_value(output_dir, Opts, ".")).

split_tokens_by_dots(Tokens) ->
    split_tokens_by_dots(Tokens, []).

split_tokens_by_dots([], Acc) ->
    lists:reverse(Acc);

split_tokens_by_dots(Tokens, Acc) ->
    {SansDot, [Dot | Rest]} = lists:splitwith(fun is_not_dot/1, Tokens),
    HasDot = SansDot ++ [Dot],
    split_tokens_by_dots(Rest, [HasDot | Acc]).

is_not_dot({dot, _}) -> false;
is_not_dot(_) -> true.

parse_forms(TokenList) ->
    parse_forms(TokenList, []).

parse_forms([], Acc) ->
    lists:reverse(Acc);

parse_forms([Tokens | Tail], Acc) ->
    {ok, Form} = erl_parse:parse_form(Tokens),
    parse_forms(Tail, [Form | Acc]).

output([], _OutputDir) ->
    ok;

output([Module | Tail], OutputDir) ->
    {ok, ModName, Bin, _Warnings} = compile:forms(Module, [return]),
    File = filename:join(OutputDir, atom_to_list(ModName) ++ ".beam"),
    ok = file:write_file(File, Bin),
    output(Tail, OutputDir).

analyze_forms(Forms, Params) ->
    analyze_forms(Forms, [], Params).

analyze_forms([], Acc, _Params) ->
    lists:reverse(Acc);

analyze_forms([Form | Forms], Acc, Params) ->
    case erl_syntax:type(Form) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
                {record, {RecordName, _RecordFields}} ->
                    SimpleFields = simplify_fields(Form),
                    Mod = create_module(RecordName, SimpleFields, Params),
                    analyze_forms(Forms, [Mod | Acc], Params);
                _ ->
                    analyze_forms(Forms, Acc, Params)
            end;
        _ ->
            analyze_forms(Forms, Acc, Params)
    end.

simplify_fields({attribute, _Line, record, {_RecName, Fields}}) ->
    simplify_fields(Fields, []).

simplify_fields([], Acc) ->
    lists:reverse(Acc);

simplify_fields([{record_field, Line, NameForm} | Tail], Acc) ->
    Name = erl_parse:normalise(NameForm),
    FieldRec = #record_field{name = Name, name_form = NameForm,
        default_form = undefined_default(Line) },
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
        typelist = Types},
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
        number, boolean, binary, atom],
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

create_module(RecordName, Fields, Params) ->
    {ok, ModuleDeclaration} = module_declaration(RecordName),
    {ok, ExportDeclaration} = export_declaration(Fields, Params),
    {ok, NewFunctions} = additional_funcs(RecordName, Fields, Params),
    [ModuleDeclaration, ExportDeclaration] ++ NewFunctions.

%additional_funcs(RecordName, Fields) ->
%    additional_funcs(RecordName, Fields, []).

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
    {ok, FieldListFunc} = get_field_names_func(Fields),
    {ok, TypeListFunc} = get_field_types_func(Fields),
    {ok, ToJsonA1} = to_json_arity1_func(),
    {ok, ToJsonA2} = to_json_arity2_func(),
    {ok, FromJsonA1} = from_json_arity1_func(RecordName, Fields),
    {ok, FromJsonA2} = from_json_arity2_func(RecordName, Fields),
    {ok, FromJsonA3} = from_json_arity3_func(),
    ScrubKeys = scrub_keys_func(Fields),
    GrandFuncList = []
        ++ AccessorFuncs
        ++ SetterFuncs
        ++ [FieldListFunc]
        ++ [TypeListFunc]
        ++ [ToJsonA1]
        ++ [ToJsonA2]
        ++ [FromJsonA1]
        ++ [FromJsonA2]
        ++ [FromJsonA3]
        ++ ScrubKeys
    , {ok, GrandFuncList}.

module_declaration(Name) ->
    {ok, {attribute, 1, module, Name}}.

export_declaration(Fields) ->
    export_declaration(Fields, []).

export_declaration(Fields, Params) ->
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
    Decs = [{field_names, 0}, {field_types, 0}, {to_json, 1}, {to_json, 2},
        {from_json, 1}, {from_json, 2}, {from_json, 3}] ++ FieldDecs ++
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

scrub_keys_func(Fields) ->
    TopScrub = {function, ?LINE, scrub_keys, 1, [
        {clause, ?LINE, [{var, ?LINE, 'Json'}], [], [
            {call, ?LINE, {atom, ?LINE, scrub_keys}, [{var, ?LINE, 'Json'}, {nil, ?LINE}]}
        ]}
    ]},
    EndScrubClause = scrub_key_end(),
    CatchBinClauses = lists:map(fun scrub_key_clause/1, Fields),
    CatchAtomClause = scrub_key_catch_atom(),
    CatchAllClause = scrub_key_catch_all(),
    AllClauses = [EndScrubClause] ++ CatchBinClauses ++ [CatchAtomClause, CatchAllClause],

    ScrubKeys = {function, ?LINE, scrub_keys, 2, AllClauses},

    [TopScrub, ScrubKeys].

scrub_key_catch_atom() ->
    KeyValueMatch = {match, ?LINE, {tuple, ?LINE, [{var, ?LINE, 'Key'}, {var, ?LINE, '_'}]}, {var, ?LINE, 'Head'}},
    Arg1 = {cons, ?LINE, KeyValueMatch, {var, ?LINE, 'Tail'}},
    Arg2 = {var, ?LINE, 'Acc'},
    ArgsList = [Arg1, Arg2],
    GuardList = [[{call, ?LINE, {atom, ?LINE, is_atom}, [{var, ?LINE, 'Key'}]}]],
    CallList = [
        {call, ?LINE, {atom, ?LINE, scrub_keys}, [
            {var, ?LINE, 'Tail'}, {cons, ?LINE, {var, ?LINE, 'Head'}, {var, ?LINE, 'Acc'}}
        ]}
    ],
    {clause, ?LINE, ArgsList, GuardList, CallList}.

scrub_key_catch_all() ->
    {clause, ?LINE, [{cons, ?LINE, {var, ?LINE, '_'}, {var, ?LINE, 'Tail'}}, {var, ?LINE, 'Acc'}], [], [
        {call, ?LINE, {atom, ?LINE, scrub_keys}, [{var, ?LINE, 'Tail'}, {var, ?LINE, 'Acc'}]}
    ]}.

scrub_key_end() ->
    {clause, ?LINE, [{nil, ?LINE}, {var, ?LINE, 'Acc'}], [], [
        {call, ?LINE, {remote, ?LINE, {atom, ?LINE, lists}, {atom, ?LINE, reverse}}, [{var, ?LINE, 'Acc'}]}
    ]}.

scrub_key_clause(FieldRec) ->
    NameAsBin = {bin, ?LINE, [
        {bin_element, ?LINE, {string, ?LINE, atom_to_list(FieldRec#record_field.name) }, default, default}
    ]},
    ArgOneHead = {tuple, ?LINE, [NameAsBin, {var, ?LINE, 'Value'}]},
    ArgOneTail = {var, ?LINE, 'Tail'},
    ArgOne = {cons, ?LINE, ArgOneHead, ArgOneTail},
    ArgsList = [ArgOne, {var, ?LINE, 'Acc'}],

    AccOnTop = {cons, ?LINE, {tuple, ?LINE, [{atom, ?LINE, FieldRec#record_field.name}, {var, ?LINE, 'Value'}]}, {var, ?LINE, 'Acc'}},
    
    RecursiveCall = {call, ?LINE, {atom, ?LINE, scrub_keys}, [{var, ?LINE, 'Tail'}, AccOnTop]},
    {clause, ?LINE, ArgsList, [], [RecursiveCall]}.

from_json_arity1_func(RecName, Fields) ->
    BlankTuple = blank_record(RecName, Fields),
    {ok, {function, ?LINE, from_json, 1, [
        {clause, ?LINE, [{var, ?LINE, 'Json'}], [], [
            {match, ?LINE, {var, ?LINE, 'Json2'}, {call, ?LINE, {atom, ?LINE, scrub_keys}, [{var, ?LINE, 'Json'}]}},
            {call, ?LINE, {remote, ?LINE, {atom, ?LINE, rec2json}, {atom, ?LINE, from_json}}, [BlankTuple, {var, ?LINE, 'Json2'}, {nil, ?LINE}]}
        ]}
    ]}}.

from_json_arity2_func(RecName, Fields) ->
    BlankRec = blank_record(RecName, Fields),
    {ok,{function, ?LINE,from_json,2, [
        from_json_arity2_clause1(BlankRec),
        from_json_arity2_clause2(),
        from_json_arity2_clause3()
    ]}}.
  
from_json_arity2_clause1(BlankRec) ->
    Args = [{var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}],
    Guards = [[
        {call, ?LINE, {atom, ?LINE, is_list}, [{var, ?LINE, 'Json'}]},
        {call, ?LINE, {atom, ?LINE, is_list}, [{var, ?LINE, 'Opts'}]}
    ]],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [BlankRec, {var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.

from_json_arity2_clause2() ->
    Args = [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}],
    Guards = [[
        {call, ?LINE, {atom, ?LINE, is_tuple}, [{var, ?LINE, 'Struct'}]}
    ]],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}, {nil, ?LINE}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.
  
from_json_arity2_clause3() ->
    Args = [{var, ?LINE, 'Json'}, {var, ?LINE, 'Struct'}],
    Guards = [],
    Expressions = [
        {call, ?LINE, {atom, ?LINE, from_json}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json'}, {nil, ?LINE}]}
    ],
    {clause, ?LINE, Args, Guards, Expressions}.

from_json_arity3_func() ->
    {ok,{function, ?LINE, from_json,3, [
        {clause, ?LINE, [{var, ?LINE, 'Struct'},{var, ?LINE, 'Json'},{var, ?LINE, 'Opts'}], [[{call, ?LINE, {atom, ?LINE, is_list},[{var, ?LINE, 'Opts'}]}]], [
            {call, ?LINE, {atom, ?LINE, from_json}, [{var, ?LINE, 'Json'}, {var, ?LINE, 'Opts'}, {var, ?LINE, 'Struct'}]}
        ]},
        {clause, ?LINE, [{var, ?LINE, 'Json'},{var, ?LINE, 'Opts'},{var, ?LINE, 'Struct'}], [], [
            {match, ?LINE, {var, ?LINE, 'Json2'}, {call, ?LINE, {atom, ?LINE, scrub_keys},[{var, ?LINE, 'Json'}]}},
            {call, ?LINE, {remote, ?LINE, {atom, ?LINE, rec2json},{atom, ?LINE, from_json}}, [{var, ?LINE, 'Struct'}, {var, ?LINE, 'Json2'}, {var, ?LINE, 'Opts'}]}
        ]}
    ]}}.

