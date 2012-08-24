-module(rec2json_compile).

-export([scan_file/2, scan_string/2]).

-define(log(Msg, Args), io:format("~s:~p " ++ Msg ++ "\n", [?MODULE, ?LINE | Args])).
-define(log(Msg), ?log(Msg, [])).

scan_file(Hrl, Opts) ->
    ?log("scan file"),
    Imports = proplists:get_value(imports_dir, Opts, []),
    {ok, Forms} = epp:parse_file(Hrl, Imports, []),
    Modules = analyze_forms(Forms),
    output(Modules, proplists:get_value(output_dir, Opts, ".")).

scan_string(Str, Opts) ->
    ?log("scan string"),
    %Imports = proplists:get_value(imports_dir, Opts, []),
    {ok, Tokens, _Line} = erl_scan:string(Str),
    {ok, Form} = erl_parse:parse_form(Tokens),
    Modules = analyze_forms([Form]),
    output(Modules, proplists:get_value(output_dir, Opts, ".")).

output([], _OutputDir) ->
    ?log("output done"),
    ok;

output([Module | Tail], OutputDir) ->
    ?log("outputing:  ~p", [Module]),
    ?log("pretty"),
    [?log("~p", [lists:flatten(erl_pp:form(F))]) || F <- Module],
    {ok, ModName, Bin, _Warnings} = compile:forms(Module, [return]),
    ?log("module:  ~p", [ModName]),
    File = filename:join(OutputDir, atom_to_list(ModName) ++ ".beam"),
    WriteRes = file:write_file(File, Bin),
    ?log("write res:  ~p", [WriteRes]),
    output(Tail, OutputDir).

analyze_forms(Forms) ->
    analyze_forms(Forms, []).

analyze_forms([], Acc) ->
    ?log("forms analysis done:  ~p", [Acc]),
    lists:reverse(Acc);

analyze_forms([Form | Forms], Acc) ->
    ?log("analyzing form:  ~p", [Form]),
    ?log("Type:  ~p", [erl_syntax:type(Form)]),
    case erl_syntax:type(Form) of
        attribute ->
            ?log("attribute analyze:  ~p", [erl_syntax_lib:analyze_attribute(Form)]),
            case erl_syntax_lib:analyze_attribute(Form) of
                {record, {RecordName, RecordFields}} ->
                    Mod = create_module(RecordName, RecordFields),
                    analyze_forms(Forms, [Mod | Acc]);
                _ ->
                    analyze_forms(Forms, Acc)
            end;
        _ ->
            analyze_forms(Forms, Acc)
    end.

create_module(RecordName, Fields) ->
    ?log("creating module"),
    {ok, ModuleDeclaration} = module_declaration(RecordName),
    {ok, ExportDeclaration} = export_declaration(Fields),
    {ok, FromOptRecDeclaration} = from_opt_rec_declaration(),
    {ok, ToOptRecDeclaration} = to_opt_rec_declaration(),
    AccessorFuncs = accessor_funcs(Fields),
    {ok, ToJsonA1} = to_json_arity1_func(Fields),
    {ok, ToJsonA2} = to_json_arity2_func(Fields),
    {ok, ToJson} = to_json_func(Fields),
    {ok, ToJsonTransform} = to_json_transform_func(),
    {ok, FromJsonA1} = from_json_arity1_func(RecordName, Fields),
    {ok, FromJsonA2} = from_json_arity2_func(RecordName, Fields),
    {ok, FromJson} = from_json_func(Fields),
    BuildFromOptRecFuncs = build_from_opt_rec_func(),
    BuildToOptRecFuncs = build_to_opt_rec_func(),
    %{ok, FromJson} = from_json_func(RecordName, Fields),
    [ModuleDeclaration, ExportDeclaration, FromOptRecDeclaration,
        ToOptRecDeclaration] ++ AccessorFuncs ++ [ToJsonA1, ToJsonA2, ToJson,
        ToJsonTransform, FromJsonA1, FromJsonA2, FromJson] ++
        BuildFromOptRecFuncs ++ BuildToOptRecFuncs.

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

atom_to_varname(Atom) ->
    ?log("atom to varname:  ~p", [Atom]),
    [Chr1 | Rest] = atom_to_list(Atom),
    [ChrUp] = string:to_upper([Chr1]),
    Variable = erl_syntax:variable([ChrUp | Rest]),
    erl_syntax:variable_name(Variable).

module_declaration(Name) ->
    String = lists:flatten(io_lib:format("-module(~p).", [Name])),
    {ok, Tokens, _Line} = erl_scan:string(String),
    erl_parse:parse_form(Tokens).

export_declaration(Fields) ->
    FieldDecs = export_declarations(Fields, []),
    Decs = ["to_json/1", "to_json/2", "from_json/1", "from_json/2" | FieldDecs],
    Decs1 = string:join(Decs, ","),
    String = lists:flatten(io_lib:format("-export([~s]).", [Decs1])),
    {ok, Tokens, _Line} = erl_scan:string(String),
    erl_parse:parse_form(Tokens).

export_declarations([], Acc) ->
    lists:reverse(Acc);

export_declarations([{K, _Type} | Tail], Acc) ->
    D = lists:flatten(io_lib:format("~p/1", [K])),
    export_declarations(Tail, [D | Acc]).

accessor_funcs(Fields) ->
    accessor_funcs(Fields, 2, []).

accessor_funcs([], _Num, Acc) ->
    lists:reverse(Acc);

accessor_funcs([{K, _Val} | Tail], N, Acc) ->
    FunctionStr = "~p(Struct) -> element(~p, Struct).",
    FunctionStr1 = lists:flatten(io_lib:format(FunctionStr, [K, N])),
    ?log("function str1:  \n~s", [FunctionStr1]),
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr1),
    {ok, Forms} = erl_parse:parse_form(Tokens),
    accessor_funcs(Tail, N + 1, [Forms | Acc]).

to_json_arity1_func(Fields) ->
    Length = length(Fields) + 1,
    FunctionStr = "to_json(Struct) -> to_json(Struct, ~p, [], []).",
    FunctionStr1 = lists:flatten(io_lib:format(FunctionStr, [Length])),
    ?log("Function str:  ~p", [FunctionStr1]),
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr1),
    erl_parse:parse_form(Tokens).

to_json_arity2_func(Fields) ->
    Length = length(Fields) + 1,
    FunctionStr = "to_json(Struct, Options) -> to_json(Struct, ~p, [], Options).",
    FunctionStr1 = lists:flatten(io_lib:format(FunctionStr, [Length])),
    parse_string(FunctionStr1).

to_json_func(Fields) ->
    Ending =
        "to_json(_Struct, 1, Acc, Options) ->"
        "   to_json_transform(Acc, Options)",
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
        "       _Else ->"
        "           to_json(Struct, Elem - 1, [{~s, Value} | Acc], Options)"
        "   end",
    Clauses = to_json_func(Fields, Looper, 2, []),
    Clauses1 = [Ending, OptionTrap] ++ Clauses,
    FunctionStr = string:join(Clauses1, ";") ++ ".",
    ?log("Function str:  ~p", [FunctionStr]),
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr),
    erl_parse:parse_form(Tokens).

to_json_func([], _LooperStr, _Elem, Acc) ->
    lists:reverse(Acc);

to_json_func([{K, _Type} | Tail], Str, Elem, Acc) ->
    Str1 = lists:flatten(io_lib:format(Str, [Elem, K, K])),
    to_json_func(Tail, Str, Elem + 1, [Str1 | Acc]).

to_json_transform_func() ->
    Func =
        "to_json_transform(Json, #to_json_opt{transforms = Trans}) ->"
        "   to_json_transform(Json, Trans);"
        "to_json_transform([], []) ->"
        "   [{}];"
        "to_json_transform(Json, []) ->"
        "   Json;"
        "to_json_transform(Json, [Func | Tail]) when is_function(Func) ->"
        "   Json2 = Func(Json),"
        "   to_json_transform(Json2, Tail);"
        "to_json_transform(Json, [Atom | Tail]) when is_atom(Atom) ->"
        "   Json2 = proplists:delete(Atom, Json),"
        "   to_json_transform(Json2, Tail);"
        "to_json_transform(Json, [{_Key, _Value} = Prop| Tail]) ->"
        "   Json2 = Json ++ [Prop],"
        "   to_json_transform(Json2, Tail).",
    parse_string(Func).

blank_record(RecName, Fields) ->
    Blanks = ["undefined" || _ <- lists:seq(1, length(Fields))],
    TupleBits = [atom_to_list(RecName) | Blanks],
    "{" ++ string:join(TupleBits, ",") ++ "}".

from_json_arity1_func(RecName, Fields) ->
    BlankTuple = blank_record(RecName, Fields),
    FromJsonA1Str = "from_json(Json) -> from_json(Json, ~s, []).",
    FromJsonA1Str1 = lists:flatten(io_lib:format(FromJsonA1Str, [BlankTuple])),
    {ok, FromJsonA1Tokens, _Line} = erl_scan:string(FromJsonA1Str1),
    %?log("from json A1 forms:  ~p", [FromJsonA1Forms]),
    erl_parse:parse_form(FromJsonA1Tokens).

from_json_arity2_func(RecName, Fields) ->
    BlankRec = blank_record(RecName, Fields),
    FromJsonA2Str = "from_json(Json, Opts) -> from_json(Json, ~s, Opts).",
    FromJsonA2Str1 = lists:flatten(io_lib:format(FromJsonA2Str, [BlankRec])),
    {ok, FromJsonA2Tokens, _Line} = erl_scan:string(FromJsonA2Str1),
    erl_parse:parse_form(FromJsonA2Tokens).

from_json_func(Fields) ->
    {ok, PropFuncs} = from_json_func(Fields, 2, []),
    ?log("from json prop funcs:  ~p", [PropFuncs]),
    {ok, PropFuncs}.

from_json_func([], _N, Acc) ->
    OptionCatcher =
        "from_json(Json, Struct, Options) when is_list(Options) ->"
        "   Options2 = build_from_opts(Options),"
        "   from_json(Json, Struct, Options2);",
    FinishedFuncStr =
        "from_json([], Struct, _Options) ->"
        "   {ok, Struct};",
    CatchallFuncStr =
        "from_json([_ | Tail], Struct, Options) ->"
        "   from_json(Tail, Struct, Options).",
    Acc0 = [CatchallFuncStr | Acc],
    Acc1 = lists:reverse(Acc0),
    Acc2 = [OptionCatcher, FinishedFuncStr | Acc1],
    Func = string:join(Acc2, []),
    ?log("from json func str:  ~n~p", [Func]),
    {ok, Tokens, _Line} = erl_scan:string(Func),
    erl_parse:parse_form(Tokens);

from_json_func([{K, none} | Tail], ElemNum, Acc) ->
    BinLabelFuncNull =
        "from_json([{<<\"~s\">>, null} | Tail], Struct, #from_json_opt{treat_null = undefined} = Opt) ->"
        "   Struct0 = setelement(~p, Struct, undefined),"
        "   from_json(Tail, Struct0, Opt);",
    BinLabelFuncUndef =
        "from_json([{<<\"~s\">>, null} | Tail], Struct, #from_json_opt{treat_null = null} = Opt) ->"
        "   Struct0 = setelement(~p, Struct, null),"
        "   from_json(Tail, Struct0, Opt);",
    AtomLabelFuncNull =
        "from_json([{~s, null} | Tail], Struct, #from_json_opt{treat_null = null} = Opt) ->"
        "   Struct0 = setelement(~p, Struct, null),"
        "   from_json(Tail, Struct0, Opt);",
    AtomLabelFuncUndef = 
        "from_json([{~s, null} | Tail], Struct, #from_json_opt{treat_null = undefined} = Opt) ->"
        "   Struct0 = setelement(~p, Struct, undefined),"
        "   from_json(Tail, Struct0, Opt);",
    BinLabelFuncStr =
        "from_json([{<<\"~s\">>, Value} | Tail], Struct, Opt) ->"
        "   Struct0 = setelement(~p, Struct, Value),"
        "   from_json(Tail, Struct0, Opt);",
    AtomLabelFuncStr =
        "from_json([{~s, Value} | Tail], Struct, Opt) ->"
        "   Struct0 = setelement(~p, Struct, Value),"
        "   from_json(Tail, Struct0, Opt);",
    FuncStrs = [BinLabelFuncNull, BinLabelFuncUndef, AtomLabelFuncNull, AtomLabelFuncUndef, BinLabelFuncStr, AtomLabelFuncStr],
    FuncStr = string:join(FuncStrs, ""),
    IoArgsList = lists:flatten([[K, ElemNum] || _ <- FuncStrs]),
    FuncStr0 = lists:flatten(io_lib:format(FuncStr, IoArgsList)),
    Acc0 = [FuncStr0 | Acc],
    from_json_func(Tail, ElemNum + 1, Acc0).






%from_json([], Struct) -> {ok, Struct};
%from_json([{<<"boolean_thing">>, Value} | Tail], Struct) ->
%    Struct0 = setelement(2, Struct, Value),
%    from_json(Tail, Struct0);
%from_json([{<<"unicode_str">>, Value} | Tail], Struct) ->
%    Struct0 = setelement(3, Struct, Value),
%    from_json(Tail, Struct0);
%from_json([{<<"count">>, Value} | Tail], Struct) ->
%    Struct0 = setelement(4, Struct, Value),
%    from_json(Tail, Struct0);
%from_json([{<<"maybe_count">>, Value} | Tail], Struct) ->
%    Struct0 = setelement(5, Struct, Value),
%    from_json(Tail, Struct0);
%from_json([_ | Tail], Struct) ->
%    from_json(Tail, Struct).
