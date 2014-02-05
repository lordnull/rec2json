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
    file:write_file(File, Bin),
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

simplify_fields([{record_field, _Line, Name} | Tail], Acc) ->
    Name2 = erl_parse:normalise(Name),
    simplify_fields(Tail, [{Name2, undefined, {any, []}} | Acc]);

simplify_fields([{record_field, _L1, Name, Default} | Tail], Acc) ->
    Name2 = erl_parse:normalise(Name),
    simplify_fields(Tail, [{Name2, Default, {any, []}} | Acc]);

simplify_fields([{typed_record_field, {record_field, _L1, Name}, Type} | Tail], Acc) ->
    Name2 = erl_parse:normalise(Name),
    Types = extract_types(Type),
    simplify_fields(Tail, [{Name2, undefined, Types} | Acc]);

simplify_fields([{typed_record_field, {record_field, _L1, Name, Default}, Type} | Tail], Acc) ->
    Name2 = erl_parse:normalise(Name),
    Types = extract_types(Type),
    simplify_fields(Tail, [{Name2, Default, Types} | Acc]).

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
    Acc2 = extract_types(Types, Acc),
    extract_types(Tail, Acc2);
extract_types([{type, _L1, list, ListTypes} | Tail], Acc) ->
    ListTypes2 = extract_types(ListTypes),
    Acc2 = [{list, ListTypes2} | Acc],
    extract_types(Tail, Acc2);
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
        true -> accessor_funcs(Fields);
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
    GrandFuncList = AccessorFuncs ++ SetterFuncs ++
				[FieldListFunc, TypeListFunc,
        ToJsonA1, ToJsonA2,
				FromJsonA1, FromJsonA2, FromJsonA3]
        ++ ScrubKeys,
    {ok, GrandFuncList}.

parse_strings(Strings) ->
    parse_strings(Strings, []).

parse_strings([], Acc) ->
    lists:reverse(Acc);

parse_strings([Str | Tail], Acc) ->
    {ok, Form} = parse_string(Str),
    parse_strings(Tail, [Form | Acc]).

parse_string(Str) ->
    {ok, Tokens, _Line} = erl_scan:string(Str),
    erl_parse:parse_form(Tokens).

module_declaration(Name) ->
    String = lists:flatten(io_lib:format("-module(~p).", [Name])),
    {ok, Tokens, _Line} = erl_scan:string(String),
    erl_parse:parse_form(Tokens).

export_declaration(Fields) ->
    export_declaration(Fields, []).
export_declaration(Fields, Params) ->
    GenerateAccessors = proplists:get_value(generate_accessors, Params, true),
    FieldDecs = case GenerateAccessors of
        true -> export_declarations(Fields, []);
        false -> []
    end,
		GenerateSetters = proplists:get_value(generate_setters, Params, true),
		SetFieldDecs = case GenerateSetters of
				true -> export_setters(Fields, []);
				false -> []
		end,
    Decs = ["field_names/0", "field_types/0", "to_json/1", "to_json/2",
        "from_json/1", "from_json/2", "from_json/3"] ++ FieldDecs ++
				SetFieldDecs,
    Decs1 = string:join(Decs, ","),
    String = lists:flatten(io_lib:format("-export([~s]).", [Decs1])),
    {ok, Tokens, _Line} = erl_scan:string(String),
    erl_parse:parse_form(Tokens).

export_declarations([], Acc) ->
    lists:reverse(Acc);

export_declarations([{K, _Default, _Types} | Tail], Acc) ->
    D = lists:flatten(io_lib:format("~p/1", [K])),
    export_declarations(Tail, [D | Acc]).

export_setters([], Acc) ->
		lists:reverse(Acc);

export_setters([{K, _Default, _Types} | Tail], Acc) ->
		D = lists:flatten(io_lib:format("~p/2", [K])),
		export_setters(Tail, [D | Acc]).

accessor_funcs(Fields) ->
    accessor_funcs(Fields, 2, []).

accessor_funcs([], _Num, Acc) ->
    lists:reverse(Acc);

accessor_funcs([{K, _Default, _Type} | Tail], N, Acc) ->
    FunctionStr = "~p(Struct) -> element(~p, Struct).",
    FunctionStr1 = lists:flatten(io_lib:format(FunctionStr, [K, N])),
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr1),
    {ok, Forms} = erl_parse:parse_form(Tokens),
    accessor_funcs(Tail, N + 1, [Forms | Acc]).

setter_funcs(Fields) ->
		setter_funcs(Fields, 2, []).

setter_funcs([], _Num, Acc) ->
		lists:reverse(Acc);

setter_funcs([{K, _Default, _Type} | Tail], N, Acc) ->
		FunctionStr = "~p(NewVal, Struct) -> setelement(~p, Struct, NewVal).",
		FuncStr1 = lists:flatten(io_lib:format(FunctionStr, [K, N])),
		{ok, Tokens, _Line} = erl_scan:string(FuncStr1),
		{ok, Forms} = erl_parse:parse_form(Tokens),
		setter_funcs(Tail, N + 1, [Forms | Acc]).

get_field_names_func(Fields) ->
    Names = [F || {F, _, _} <- Fields],
    Str = lists:flatten(io_lib:format("field_names() -> ~p.", [Names])),
    parse_string(Str).

get_field_types_func(Fields) ->
    TypeProps = lists:map(fun({FieldName, _Default, {Anyness, Types}}) ->
        TypeProp = exportable_types(Types),
        {FieldName, {Anyness, TypeProp}}
      end, Fields),
      FuncStr = "field_types() -> ~p.",
      Str = lists:flatten(io_lib:format(FuncStr, [TypeProps])),
      parse_string(Str).

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
    FunctionStr = "to_json(Struct) -> rec2json:to_json(Struct, []).",
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr),
    erl_parse:parse_form(Tokens).

to_json_arity2_func() ->
    FunctionStr =
        "to_json(Struct, Options) when is_tuple(Struct) ->"
        "    rec2json:to_json(Struct, Options);"
        "to_json(Options, Struct) ->"
        "    to_json(Struct, Options).",
    parse_string(FunctionStr).

blank_record(RecName, Fields) ->
    Defaults = [printable_default(D) || {_,D,_} <- Fields],
    TupleBits = [RecName | Defaults],
    Tuple = list_to_tuple(TupleBits),
    lists:flatten(io_lib:format("~p", [Tuple])).

printable_default({call, _L1, ModFunc, Args}) ->
    ModFunc2 = printable_modfunc(ModFunc),
    Args2 = [printable_default(Arg) || Arg <- Args],
    Args3 = insert_commas(Args2),
    io_lib:format("~p(~p)", [ModFunc2, Args3]);

printable_default(undefined) ->
    undefined;

printable_default(Abstract) ->
    erl_parse:normalise(Abstract).

printable_modfunc({remote, _, {atom, _, Mod}, {atom, _, Func}}) ->
    io_lib:format("~p:~p", [Mod, Func]);
printable_modfunc({atom, _, Func}) ->
    io_lib:format("~p", [Func]).

insert_commas(List) ->
    insert_commas(List, []).

insert_commas([], []) ->
    [];
insert_commas([Last], Acc) ->
    lists:reverse([Last, $, | Acc]);
insert_commas([Head | Tail], Acc) ->
    Acc2 = [Head, $, | Acc],
    insert_commas(Tail, Acc2).

scrub_keys_func(Fields) ->
    Func =
        "scrub_keys(Json) ->"
        "   scrub_keys(Json, []).",
    Scrubbing = scrub_keys_func(Fields, []),
    parse_strings([Func, Scrubbing]).

scrub_keys_func([], Acc) ->
    Catchall =
        "scrub_keys([_Head | Tail], Acc) ->"
        "   scrub_keys(Tail, Acc).",
    CatchAtom =
        "scrub_keys([{Key, _} = Head | Tail], Acc) when is_atom(Key) ->"
        "   scrub_keys(Tail, [Head | Acc])",
    Ending =
        "scrub_keys([], Acc) ->"
        "   lists:reverse(Acc)",
    Acc2 = [Catchall, CatchAtom | Acc],
    Acc3 = [Ending | lists:reverse(Acc2)],
    string:join(Acc3, ";");

scrub_keys_func([{Name, _Default, _Types} | Tail], Acc) ->
    ClauseStr =
        "scrub_keys([{<<\"~s\">>, Val} | Tail], Acc) ->"
        "   scrub_keys(Tail, [{~s, Val} | Acc])",
    ClauseStr2 = lists:flatten(io_lib:format(ClauseStr, [Name, Name])),
    scrub_keys_func(Tail, [ClauseStr2 | Acc]).

from_json_arity1_func(RecName, Fields) ->
    BlankTuple = blank_record(RecName, Fields),
    FromJsonA1Str =
        "from_json(Json) ->"
        "   Json2 = scrub_keys(Json),"
        "   rec2json:from_json(~s, Json2, []).",
    FromJsonA1Str1 = lists:flatten(io_lib:format(FromJsonA1Str, [BlankTuple])),
    {ok, FromJsonA1Tokens, _Line} = erl_scan:string(FromJsonA1Str1),
    erl_parse:parse_form(FromJsonA1Tokens).

from_json_arity2_func(RecName, Fields) ->
    BlankRec = blank_record(RecName, Fields),
    FromJsonA2Str =
        "from_json(Json, Opts) when is_list(Json), is_list(Opts)->"
        "   from_json(~s, Json, Opts);"
        "from_json(Struct, Json) when is_tuple(Struct) ->"
        "    from_json(Struct, Json, []);"
        "from_json(Json, Struct) ->"
        "   from_json(Struct, Json, []).",
    FromJsonA2Str1 = lists:flatten(io_lib:format(FromJsonA2Str, [BlankRec])),
    {ok, FromJsonA2Tokens, _Line} = erl_scan:string(FromJsonA2Str1),
    erl_parse:parse_form(FromJsonA2Tokens).

from_json_arity3_func() ->
    FromJsonA3Str =
        "from_json(Struct, Json, Opts) when is_list(Opts) ->"
        "    from_json(Json, Opts, Struct);"
        "from_json(Json, Opts, Struct) ->"
        "    Json2 = scrub_keys(Json),"
        "    rec2json:from_json(Struct, Json2, Opts).",
    {ok, FromJsonA3Tokens, _Line} = erl_scan:string(FromJsonA3Str),
    erl_parse:parse_form(FromJsonA3Tokens).
