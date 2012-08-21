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
    {ok, Tokens} = erl_scan:string(Str),
    Forms = erl_parse:parse_form(Tokens),
    Modules = analyze_forms(Forms),
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
    AccessorFuncs = accessor_funcs(Fields),
    {ok, ToJson} = to_json_func(Fields),
    {ok, FromJsonA1} = from_json_arity1_func(RecordName, Fields),
    {ok, FromJson} = from_json_func(Fields),
    %{ok, FromJson} = from_json_func(RecordName, Fields),
    [ModuleDeclaration, ExportDeclaration] ++ AccessorFuncs ++ [ToJson, FromJsonA1, FromJson].
    
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
    Decs = ["to_json/1", "from_json/1" | FieldDecs],
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

to_json_func(Fields) ->
    Body = to_json_func(Fields, 2, []),
    FunctionStr = "to_json(Struct) -> [~s].",
    FunctionStr1 = lists:flatten(io_lib:format(FunctionStr, [Body])),
    ?log("Function str:  ~p", [FunctionStr1]),
    {ok, Tokens, _Line} = erl_scan:string(FunctionStr1),
    erl_parse:parse_form(Tokens).

to_json_func([], _Num, Acc) ->
    string:join(lists:reverse(Acc), ",");

to_json_func([{K, _Type} | Tail], Num, Acc) ->
    KvString = "{~p, element(~p, Struct)}",
    KvString1 = lists:flatten(io_lib:format(KvString, [K, Num])),
    to_json_func(Tail, Num + 1, [KvString1 | Acc]).

from_json_arity1_func(RecName, Fields) ->
    Blanks = ["undefined" || _ <- lists:seq(1, length(Fields))],
    TupleBits = [atom_to_list(RecName) | Blanks],
    BlankTuple = "{" ++ string:join(TupleBits, ",") ++ "}",
    FromJsonA1Str = "from_json(Json) -> from_json(Json, ~s).",
    FromJsonA1Str1 = lists:flatten(io_lib:format(FromJsonA1Str, [BlankTuple])),
    {ok, FromJsonA1Tokens, _Line} = erl_scan:string(FromJsonA1Str1),
    %?log("from json A1 forms:  ~p", [FromJsonA1Forms]),
    erl_parse:parse_form(FromJsonA1Tokens).

from_json_func(Fields) ->
    {ok, PropFuncs} = from_json_func(Fields, 2, []),
    ?log("from json prop funcs:  ~p", [PropFuncs]),
    {ok, PropFuncs}.

from_json_func([], _N, Acc) ->
    CatchallFuncStr = "from_json([_ | Tail], Struct) -> from_json(Tail, Struct).",
    FinishedFuncStr = "from_json([], Struct) -> {ok, Struct};\n",
    Acc0 = [CatchallFuncStr | Acc],
    Acc1 = lists:reverse(Acc0),
    Acc2 = [FinishedFuncStr | Acc1],
    Func = string:join(Acc2, []),
    ?log("from json func str:  ~n~p", [Func]),
    {ok, Tokens, _Line} = erl_scan:string(Func),
    erl_parse:parse_form(Tokens);

from_json_func([{K, _Type} | Tail], ElemNum, Acc) ->
    FuncStr = "from_json([{<<\"~s\">>, Value} | Tail], Struct) -> Struct0 = setelement(~p, Struct, Value), from_json(Tail, Struct0);~n",
    FuncStr0 = lists:flatten(io_lib:format(FuncStr, [K, ElemNum])),
    Acc0 = [FuncStr0 | Acc],
    from_json_func(Tail, ElemNum + 1, Acc0).






from_json([], Struct) -> {ok, Struct};
from_json([{<<"boolean_thing">>, Value} | Tail], Struct) ->
    Struct0 = setelement(2, Struct, Value),
    from_json(Tail, Struct0);
from_json([{<<"unicode_str">>, Value} | Tail], Struct) ->
    Struct0 = setelement(3, Struct, Value),
    from_json(Tail, Struct0);
from_json([{<<"count">>, Value} | Tail], Struct) ->
    Struct0 = setelement(4, Struct, Value),
    from_json(Tail, Struct0);
from_json([{<<"maybe_count">>, Value} | Tail], Struct) ->
    Struct0 = setelement(5, Struct, Value),
    from_json(Tail, Struct0);
from_json([_ | Tail], Struct) ->
    from_json(Tail, Struct).
