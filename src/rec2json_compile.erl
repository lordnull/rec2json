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

-module(rec2json_compile).

-export([scan_file/2, scan_string/2]).
-export([simplify_fields/1, export_declaration/1,
    opt_record_declarations/0, additional_funcs/2]).

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
    Modules = analyze_forms(Forms),
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
    Modules = analyze_forms(Forms),
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

analyze_forms(Forms) ->
    analyze_forms(Forms, []).

analyze_forms([], Acc) ->
    lists:reverse(Acc);

analyze_forms([Form | Forms], Acc) ->
    case erl_syntax:type(Form) of
        attribute ->
            case erl_syntax_lib:analyze_attribute(Form) of
                {record, {RecordName, _RecordFields}} ->
                    SimpleFields = simplify_fields(Form),
                    Mod = create_module(RecordName, SimpleFields),
                    analyze_forms(Forms, [Mod | Acc]);
                _ ->
                    analyze_forms(Forms, Acc)
            end;
        _ ->
            analyze_forms(Forms, Acc)
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
    Default2 = erl_parse:normalise(Default),
    simplify_fields(Tail, [{Name2, Default2, {any, []}} | Acc]);

simplify_fields([{typed_record_field, {record_field, _L1, Name}, Type} | Tail], Acc) ->
    Name2 = erl_parse:normalise(Name),
    Types = extract_types(Type),
    simplify_fields(Tail, [{Name2, undefined, Types} | Acc]);

simplify_fields([{typed_record_field, {record_field, _L1, Name, Default}, Type} | Tail], Acc) ->
    Name2 = erl_parse:normalise(Name),
    Types = extract_types(Type),
    Default2 = erl_parse:normalise(Default),
    simplify_fields(Tail, [{Name2, Default2, Types} | Acc]).

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
            extract_types(Tail, Acc)
    end;
extract_types([Type | Tail], Acc) ->
    Acc2 = [erl_parse:normalise(Type) | Acc],
    extract_types(Tail, Acc2).

supported_type(Type, []) ->
    Supported = [integer, pos_integer, non_neg_integer, neg_integer, float,
        number, boolean, binary],
    lists:member(Type, Supported);

supported_type(record, _) ->
    % no go way to know if the record is rec2json compiled or not; just have to
    % trust the user.
    true.

create_module(RecordName, Fields) ->
    {ok, ModuleDeclaration} = module_declaration(RecordName),
    {ok, ExportDeclaration} = export_declaration(Fields),
    {ok, OptRecDeclarations} = opt_record_declarations(),
    %{ok, FromOptRecDeclaration} = from_opt_rec_declaration(),
    %{ok, ToOptRecDeclaration} = to_opt_rec_declaration(),
    {ok, NewFunctions} = additional_funcs(RecordName, Fields),
    [ModuleDeclaration, ExportDeclaration] ++ OptRecDeclarations ++
        NewFunctions.

additional_funcs(RecordName, Fields) ->
    AccessorFuncs = accessor_funcs(Fields),
    {ok, ToJsonA1} = to_json_arity1_func(),
    {ok, ToJsonA2} = to_json_arity2_func(Fields),
    {ok, ToJson} = to_json_func(Fields),
    {ok, ToJsonTransform} = to_json_transform_func(),
    {ok, FromJsonA1} = from_json_arity1_func(RecordName, Fields),
    {ok, FromJsonA2} = from_json_arity2_func(RecordName, Fields),
    {ok, FromJsonA3} = from_json_arity3_func(),
    {ok, FromJson} = from_json_func(Fields),
    ScrubKeys = scrub_keys_func(Fields),
    BuildFromOptRecFuncs = build_from_opt_rec_func(),
    BuildToOptRecFuncs = build_to_opt_rec_func(),
    GrandFuncList =  AccessorFuncs ++ [ToJsonA1, ToJsonA2, ToJson,
        ToJsonTransform, FromJsonA1, FromJsonA2, FromJsonA3,
        FromJson] ++ ScrubKeys ++ BuildFromOptRecFuncs ++
        BuildToOptRecFuncs,
    {ok, GrandFuncList}.

opt_record_declarations() ->
    {ok, ToJsonOptRec} = to_opt_rec_declaration(),
    {ok, FromJsonOptRec} = from_opt_rec_declaration(),
    {ok, [ToJsonOptRec, FromJsonOptRec]}.

from_opt_rec_declaration() ->
    Str = "-record(from_json_opt, {treat_null = null}).",
    {ok, Tokens, _Line} = erl_scan:string(Str),
    erl_parse:parse_form(Tokens).

build_from_opt_rec_func() ->
    FuncA1 = "build_from_opts(Opts) -> build_from_opts(Opts, #from_json_opt{}).",
    FuncA2 =
        "build_from_opts([], Rec) ->"
        "   Rec;"
        "build_from_opts([null_is_undefined |Tail], Rec) ->"
        "   Rec2 = Rec#from_json_opt{treat_null = undefined},"
        "   build_from_opts(Tail, Rec2);"
        "build_from_opts([{null_is_undefined, true} | Tail], Rec) ->"
        "   Rec2 = Rec#from_json_opt{treat_null = undefined},"
        "   build_from_opts(Tail, Rec2);"
        "build_from_opts([{null_is_undefined, false} | Tail], Rec) ->"
        "   Rec2 = Rec#from_json_opt{treat_null = null},"
        "   build_from_opts(Tail, Rec2).",
    {ok, FuncA1T, _} = erl_scan:string(FuncA1),
    {ok, FuncA2T, _} = erl_scan:string(FuncA2),
    {ok, FuncA1F} = erl_parse:parse_form(FuncA1T),
    {ok, FuncA2F} = erl_parse:parse_form(FuncA2T),
    [FuncA1F, FuncA2F].

to_opt_rec_declaration() ->
    Str = "-record(to_json_opt, {treat_undefined = skip, transforms = []}).",
    {ok, Tokens, _Line} = erl_scan:string(Str),
    erl_parse:parse_form(Tokens).

build_to_opt_rec_func() ->
    FuncA1 =
        "build_to_opts(Opts) ->"
        "   build_to_opts(Opts, #to_json_opt{}).",
    FuncA2 =
        "build_to_opts([], Rec) ->"
        "   Transforms = lists:reverse(Rec#to_json_opt.transforms),"
        "   Rec#to_json_opt{transforms = Transforms};"
        "build_to_opts([{null_is_undefined} | Tail], Rec) ->"
        "   Rec2 = Rec#to_json_opt{treat_undefined = null},"
        "   build_to_opts(Tail, Rec2);"
        "build_to_opts([Transform | Tail], Rec) ->"
        "   Transforms1 = [Transform | Rec#to_json_opt.transforms],"
        "   Rec2 = Rec#to_json_opt{transforms = Transforms1},"
        "   build_to_opts(Tail, Rec2).",
    parse_strings([FuncA1, FuncA2]).

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
    FieldDecs = export_declarations(Fields, []),
    Decs = ["to_json/1", "to_json/2", "from_json/1", "from_json/2",
        "from_json/3" | FieldDecs],
    Decs1 = string:join(Decs, ","),
    String = lists:flatten(io_lib:format("-export([~s]).", [Decs1])),
    {ok, Tokens, _Line} = erl_scan:string(String),
    erl_parse:parse_form(Tokens).

export_declarations([], Acc) ->
    lists:reverse(Acc);

export_declarations([{K, _Default, _Types} | Tail], Acc) ->
    D = lists:flatten(io_lib:format("~p/1", [K])),
    export_declarations(Tail, [D | Acc]).

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

to_json_arity1_func() ->
    FunctionStr = "to_json(Struct) -> to_json(Struct, []).",
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr),
    erl_parse:parse_form(Tokens).

to_json_arity2_func(Fields) ->
    FieldNamesStr = [ atom_to_list(Fname) || {Fname, _, _} <- Fields ],
    FieldNamesStr1 = string:join(FieldNamesStr, ","),
    FunctionStr =
        "to_json(Struct, Options) when is_tuple(Struct) ->"
        "    rec2json:to_json(Struct, [~s], Options);"
        "to_json(Options, Struct) ->"
        "    to_json(Struct, Options).",
    FunctionStr1 = lists:flatten(io_lib:format(FunctionStr, [FieldNamesStr1])),
    parse_string(FunctionStr1).

to_json_func(Fields) ->
    Ending =
        "to_json(Struct, 1, Acc, Options) ->"
        "   to_json_transform(Acc, Struct, Options)",
    OptionTrap =
        "to_json(Struct, Elem, Acc, Options) when is_list(Options) ->"
        "   OptRec = build_to_opts(Options),"
        "   to_json(Struct, Elem, Acc, OptRec)",
    Looper =
        "to_json(Struct, ~p = Elem, Acc, Options) ->"
        "   #to_json_opt{treat_undefined = UndefDo} = Options,"
        "   Value = element(Elem, Struct),"
        "   case {Value, UndefDo} of"
        "       {undefined, skip} ->"
        "           to_json(Struct, Elem - 1, Acc, Options);"
        "       {undefined, null} ->"
        "           to_json(Struct, Elem - 1, [{~s, null} | Acc], Options);"
        "       {Tuple, _} when is_tuple(Tuple) ->"
        "           SubRecName = element(1, Tuple),"
        "           case erlang:function_exported(SubRecName, to_json, 2) of"
        "               false ->"
        "                   erlang:error(badarg);"
        "               true ->"
        "                   SubJson = SubRecName:to_json(Tuple, Options),"
        "                   to_json(Struct, Elem - 1, [{~s, SubJson} | Acc], Options)"
        "           end;"
        "       _Else ->"
        "           to_json(Struct, Elem - 1, [{~s, Value} | Acc], Options)"
        "   end",
    Clauses = to_json_func(Fields, Looper, 2, []),
    Clauses1 = [Ending, OptionTrap] ++ Clauses,
    FunctionStr = string:join(Clauses1, ";") ++ ".",
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr),
    erl_parse:parse_form(Tokens).

to_json_func([], _LooperStr, _Elem, Acc) ->
    lists:reverse(Acc);

to_json_func([{K, _Default, _Types} | Tail], Str, Elem, Acc) ->
    Str1 = lists:flatten(io_lib:format(Str, [Elem, K, K, K])),
    to_json_func(Tail, Str, Elem + 1, [Str1 | Acc]).

to_json_transform_func() ->
    Func =
        "to_json_transform(Json, Struct, #to_json_opt{transforms = Trans}) ->"
        "   to_json_transform(Json, Struct, Trans);"
        "to_json_transform([], _Struct, []) ->"
        "   [{}];"
        "to_json_transform(Json, _Struct, []) ->"
        "   Json;"
        "to_json_transform(Json, Struct, [Func | Tail]) when is_function(Func) ->"
        "   Json2 = case erlang:fun_info(Func, arity) of"
        "       {arity, 1} ->"
        "           Func(Json);"
        "       {arity, 2} ->"
        "           Func(Json, Struct)"
        "   end,"
        "   to_json_transform(Json2, Struct, Tail);"
        "to_json_transform(Json, Struct, [Atom | Tail]) when is_atom(Atom) ->"
        "   Json2 = proplists:delete(Atom, Json),"
        "   to_json_transform(Json2, Struct, Tail);"
        "to_json_transform(Json, Struct, [{_Key, _Value} = Prop| Tail]) ->"
        "   Json2 = Json ++ [Prop],"
        "   to_json_transform(Json2, Struct, Tail).",
    parse_string(Func).

blank_record(RecName, Fields) ->
    Defaults = [D || {_,D,_} <- Fields],
    TupleBits = [RecName | Defaults],
    Tuple = list_to_tuple(TupleBits),
    lists:flatten(io_lib:format("~p", [Tuple])).

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
        "   from_json(Json2, ~s, [], []).",
    FromJsonA1Str1 = lists:flatten(io_lib:format(FromJsonA1Str, [BlankTuple])),
    {ok, FromJsonA1Tokens, _Line} = erl_scan:string(FromJsonA1Str1),
    erl_parse:parse_form(FromJsonA1Tokens).

from_json_arity2_func(RecName, Fields) ->
    BlankRec = blank_record(RecName, Fields),
    FromJsonA2Str =
        "from_json(Json, Opts) when is_list(Json), is_list(Opts)->"
        "   Json2 = scrub_keys(Json),"
        "   from_json(Json2, ~s, Opts, []);"
        "from_json(Struct, Json) when is_tuple(Struct) ->"
        "    Json2 = scrub_keys(Json),"
        "    from_json(Json2, Struct, [], []);"
        "from_json(Json, Opt) when is_record(Opt, from_json_opt) ->"
        "   from_json(Json, ~s, Opt, []);"
        "from_json(Json, Struct) ->"
        "   Json2 = scrub_keys(Json),"
        "   from_json(Json2, Struct, [], []).",
    FromJsonA2Str1 = lists:flatten(io_lib:format(FromJsonA2Str, [BlankRec, BlankRec])),
    {ok, FromJsonA2Tokens, _Line} = erl_scan:string(FromJsonA2Str1),
    erl_parse:parse_form(FromJsonA2Tokens).

from_json_arity3_func() ->
    FromJsonA3Str =
        "from_json(Struct, Json, Opts) when is_list(Opts) ->"
        "    Json2 = scrub_keys(Json),"
        "    from_json(Json2, Struct, Opts, []);"
        "from_json(Json, Opts, Struct) ->"
        "    Json2 = scrub_keys(Json),"
        "    from_json(Json2, Struct, Opts, []).",
    {ok, FromJsonA3Tokens, _Line} = erl_scan:string(FromJsonA3Str),
    erl_parse:parse_form(FromJsonA3Tokens).

from_json_func(Fields) ->
    OptionCatcher =
        "from_json(Json, Struct, Options, Warns) when is_list(Options) ->"
        "   Options2 = build_from_opts(Options),"
        "   from_json(Json, Struct, Options2, Warns)",
    FinishedFuncStr =
        "from_json([], Struct, _Options, Warns) ->"
        "   case Warns of"
        "       [] ->"
        "           {ok, Struct};"
        "       _ ->"
        "           {ok, Struct, Warns}"
        "   end",
    Acc = [FinishedFuncStr, OptionCatcher],
    {ok, PropFuncs} = from_json_func(Fields, 2, Acc),
    {ok, PropFuncs}.

from_json_func([], _N, Acc) ->
    CatchAll =
        "from_json([_|Tail], Struct, Options, Warns) ->"
        "   from_json(Tail, Struct, Options, Warns)",
    Acc2 = lists:append(Acc, [CatchAll]),
    Func = string:join(Acc2, ";\n") ++ ".",
    {ok, Tokens, _Line} = erl_scan:string(Func),
    erl_parse:parse_form(Tokens);

from_json_func([{K, _Default, Types} | Tail], ElemNum, Acc) ->
    Clause = from_json_type_clause(K, Types, ElemNum),
    from_json_func(Tail, ElemNum + 1, [Clause | Acc]).

from_json_type_clause(Key, any, ElemNum) ->
    from_json_type_clause(Key, {any, []}, ElemNum);

from_json_type_clause(Key, {Any, Types}, ElemNum) ->
    KeyClause = 
    "from_json([{~s, Val} | Tail], Struct, #from_json_opt{treat_null = NullIs} = Opt, Warns) ->"
    "   case rec2json:verify_type(Val, ~p, ~s, NullIs, Opt) of"
    "       {ok, Val1} ->"
    "           Struct0 = setelement(~p, Struct, Val1),"
    "           from_json(Tail, Struct0, Opt, Warns);"
    "       {warn, Val1} ->"
    "           Struct0 = setelement(~p, Struct, Val1),"
    "           from_json(Tail, Struct0, Opt, [~s | Warns]);"
    "       {warn, Val1, SubWarns} ->"
    "           Struct0 = setelement(~p, Struct, Val1),"
    "           SubWarns1 = case is_list(hd(SubWarns)) of"
    "               true ->"
    "                   [[~s | SW] || SW <- SubWarns];"
    "               false ->"
    "                   [[~s, SW] || SW <- SubWarns]"
    "           end,"
    "           from_json(Tail, Struct0, Opt, SubWarns1 ++ Warns)"
    "   end",
    % TODO make types usable in the string format.
    TypeStr = Types,
    lists:flatten(io_lib:format(KeyClause, [Key, TypeStr, Any, ElemNum, ElemNum, Key, ElemNum, Key, Key])).
