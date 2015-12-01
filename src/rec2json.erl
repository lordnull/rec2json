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

-module(rec2json).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(log(Msg, Args), ?debugFmt(Msg, Args)).
-define(log(Msg), ?log(Msg, [])).
-else.
-define(log(Msg, Args), io:format("~s:~p " ++ Msg ++ "\n", [?MODULE, ?LINE | Args])).
-define(log(Msg), ?log(Msg, [])).
-endif.

-export([parse_transform/2]).
-export([verify_type/5, verify_types/5]).
-export([to_json/1, to_json/2]).
-export([from_json/3]).

%% ---------------------------------------------------------------------------
%% parse transform
%% ---------------------------------------------------------------------------

parse_transform(RawForms, Options) ->
    Params = case proplists:get_value(rec2json, Options) of
        undefined -> [];
        P -> P
    end,
    %% always kinda icky to use an undocumented function.
    %% however, without this, the abstract record forms passed in lack the
    %% type information needed for user defined types to work properly.
    Forms = epp:restore_typed_record_fields(RawForms),
    ModuleName = hd([Mod || {attribute, _Line, module, Mod} <- Forms]),
    MaybeRecords = [R || {attribute, _Line, record, {RecordName, _Fields}} = R <- Forms, ModuleName == RecordName],
    case MaybeRecords of
        [Record] ->
            SimpleFields = r2j_compile:simplify_fields(Record),
            {ok, AdditionalExports} = r2j_compile:export_declaration(SimpleFields, Params),
            {ok, Functions} = r2j_compile:additional_funcs(ModuleName, SimpleFields, Params),
            WithNewBits = insert_new_bits(Forms, AdditionalExports, Functions, Params),
            lists:map(fun maybe_normalize_record_def/1, WithNewBits);
        _Records ->
            Forms
    end.

maybe_normalize_record_def({attribute, Line, record, {RecordName, RecordFields}} = Form) ->
    case epp:normalize_typed_record_fields(RecordFields) of
        not_typed ->
            Form;
        {typed, NewFields} ->
            {attribute, Line, record, {RecordName, NewFields}}
    end;

maybe_normalize_record_def(Form) ->
    Form.

insert_new_bits(Forms, AllNewExports, AllNewFunctions, Params) ->
    EofSplit = fun
        ({eof, _}) -> false;
        (_) -> true
    end,
    ExportSplit = fun
        ({attribute, _Line, export, _Exports}) ->
            false;
        (_) ->
            true
    end,
    FunctionsSplit = fun
        ({function, _Line, _Name, _Arity, _Clauses}) ->
            false;
        (_) ->
            true
    end,
    {NoEof, Eof} = lists:splitwith(EofSplit, Forms),
    {UpToFunctions, OrigFunctions} = lists:splitwith(FunctionsSplit, NoEof),
    {UpToExport, ExportAndFunctions} = lists:splitwith(ExportSplit, UpToFunctions),
    {Exports, Functions} = case proplists:get_value(careful, Params, true) of
        false ->
            {AllNewExports, AllNewFunctions};
        true ->
            {careful_exports(AllNewExports, Forms), careful_functions(AllNewFunctions, Forms)}
    end,
    UpToExport ++ [Exports] ++ ExportAndFunctions ++ Functions ++ OrigFunctions ++ Eof.

careful_exports(NewExportDecl, AllForms) ->
    {attribute, Line, export, NewExports} = NewExportDecl,
    OrigDecl = lists:foldl(fun
        ({attribute, _, export, Exp}, Acc) ->
            Exp ++ Acc;
        (_, Acc) ->
            Acc
    end, [], AllForms),
    SafeExports = NewExports -- OrigDecl,
    {attribute, Line, export, SafeExports}.

careful_functions(NewFunctions, AllForms) ->
    ExistingFuncs = lists:foldl(fun
        ({function, _Line, Name, Arity, _Clauses}, Acc) ->
            [{Name, Arity} | Acc];
        (_, Acc) ->
            Acc
    end, [], AllForms),
    lists:filter(fun({function, _Line, Name, Arity, _Clauses}) ->
        not lists:member({Name, Arity}, ExistingFuncs)
    end, NewFunctions).

%% ---------------------------------------------------------------------------
%% to json
%% ---------------------------------------------------------------------------

to_json(Tuple) ->
    to_json(Tuple, []).

to_json(Tuple, Options) when is_tuple(Tuple) ->
    {TreatUndef, Transforms} = extract_to_json_opts(Options),
    [Module | Values] = tuple_to_list(Tuple),
    Types = Module:field_types(),
    ZippedFull = lists:zip(Values, Types),
    SkippingFields = lists:filter(fun erlang:is_atom/1, Transforms),
    Zipped = lists:filter(fun(Elem) ->
        {_Value, {Name, _FTypes}} = Elem,
        not lists:member(Name, SkippingFields)
    end, ZippedFull),
    {TreatUndef, ReversedJsonProps} = lists:foldl(fun
        ({undefined, {_Name, _FTypes}}, {skip, Acc}) ->
            {skip, Acc};
        ({undefined, {Name, _FTypes}}, {null, Acc}) ->
            {null, [{Name, null} | Acc]};
        ({Value, {Name, FTypes}}, {UndefIs, Acc}) ->
            case maybe_convertable(Value, FTypes, Options) of
                error ->
                    erlang:error({badarg, {Name, Value, FTypes}});
                {ok, _NewVal, _Warns} ->
                    erlang:error({badarg, {Name, Value, FTypes}});
                {ok, undefined} when UndefIs =:= skip ->
                    {UndefIs, Acc};
                {ok, undefined} when UndefIs =:= null ->
                    {UndefIs, [{Name, null} | Acc]};
                {ok, NewVal} when is_boolean(NewVal) ->
                    {UndefIs, [{Name, NewVal} | Acc]};
                {ok, NewVal} when is_atom(NewVal) ->
                    {UndefIs, [{Name, list_to_binary(atom_to_list(NewVal))} | Acc]};
                {ok, NewVal} ->
                    {UndefIs, [{Name, NewVal} | Acc]}
            end
    end, {TreatUndef, []}, Zipped),
    UntransformedJson = lists:reverse(ReversedJsonProps),
    to_json_apply_transformations(Tuple, UntransformedJson, Transforms).

from_json(SeedTuple, [{}], _Options) ->
    {ok, SeedTuple};

from_json(SeedTuple, Json, Options) ->
    Module = element(1, SeedTuple),
    Names = Module:field_names(),
    Types = Module:field_types(),
    Elems = lists:seq(2, length(Names) + 1),
    NameBins = [list_to_binary(atom_to_list(N)) || N <- Names],
    Zipper = fun(Name, TypeList, Elem) ->
        {Name, {Elem, TypeList}}
    end,
    Zipped = lists:zipwith3(Zipper, NameBins, Types, Elems),
    TreatNull = proplists:get_value(null_is_undefined, Options, false),
    from_json(Zipped, Json, TreatNull, SeedTuple, []).

from_json(_Zipped, [], _TreatNull, Tuple, []) ->
    {ok, Tuple};

from_json(_Zipped, [], _TreatNull, Tuple, Warnings) ->
    {ok, Tuple, lists:reverse(Warnings)};

from_json(Zipped, [{NameAtom, Value} | Json], TreatNull, Tuple, Warnings) when is_atom(NameAtom) ->
    NameBin = list_to_binary(atom_to_list(NameAtom)),
    from_json(Zipped, [{NameBin, Value} | Json], TreatNull, Tuple, Warnings);

from_json(Zipped, [{Name, null} | Json], true, Tuple, Warnings) ->
    from_json(Zipped, [{Name, undefined} | Json], true, Tuple, Warnings);

from_json(Zipped, [{Name, Value} | Json], TreatNull, Tuple, Warnings) ->
    case proplists:get_value(Name, Zipped) of
        undefined ->
            from_json(Zipped, Json, TreatNull, Tuple, Warnings);
        {Elem, Types} ->
            Options = if
                TreatNull ->
                    [null_is_undefined];
                true ->
                    []
            end,
            case maybe_convertable(Value, Types, Options) of
                error ->
                    Tuple2 = setelement(Elem, Tuple, Value),
                    Warnings2 = [list_to_atom(binary_to_list(Name)) | Warnings],
                    from_json(Zipped, Json, TreatNull, Tuple2, Warnings2);
                {ok, Value2} ->
                    Tuple2 = setelement(Elem, Tuple, Value2),
                    from_json(Zipped, Json, TreatNull, Tuple2, Warnings);
                {ok, Value2, SubWarns} ->
                    Tuple2 = setelement(Elem, Tuple, Value2),
                    Warnings2 = lists:foldl(fun(SubWarn, Warns) ->
                        NameAtom = list_to_atom(binary_to_list(Name)),
                        [[NameAtom, SubWarn] | Warns]
                    end, Warnings, SubWarns),
                    from_json(Zipped, Json, TreatNull, Tuple2, Warnings2)
            end
    end.

maybe_convertable(Value, any, Options) ->
    maybe_convertable(Value, {any, []}, Options);

maybe_convertable(Value, {AnyNess, Types}, Options) ->
    ConvertRes = maybe_convertable(Value, Types, Options),
    case {ConvertRes, AnyNess} of
        {error, specific} ->
            error;
        {error, any} ->
            {ok, Value};
        {Else, _} ->
            Else
    end;

maybe_convertable(_Value, [], _Options) ->
    error;

maybe_convertable(Value, [{Mod, Func, Args} | Types], Options) ->
    Arity = length(Args) + 1,
    {module, _} = code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, Func, Arity) of
        true ->
            case erlang:apply(Mod, Func, [Value | Args]) of
                error ->
                    maybe_convertable(Value, Types, Options);
                Else ->
                    Else
            end;
        false ->
            maybe_convertable(Value, Types, Options)
    end;

maybe_convertable(null, [undefined | Types], Options) ->
    case proplists:get_value(null_is_undefined, Options, false) of
        true ->
            {ok, undefined};
        false ->
            maybe_convertable(null, Types, Options)
    end;

maybe_convertable(null, [null | _Types], _Options) ->
    {ok, null};

maybe_convertable(Value, [Atom | Types], Options) when is_atom(Atom) ->
    case r2j_type:atom(Value, Atom) of
        error ->
            maybe_convertable(Value, Types, Options);
        Else ->
            Else
    end;

maybe_convertable(Tuple, [{record, RecName} | Types], Options) when is_tuple(Tuple) andalso element(1, Tuple) =:= RecName ->
    try RecName:to_json(Tuple, Options) of
        Json ->
            {ok, Json}
    catch
        error:undef ->
            maybe_convertable(Tuple, Types, Options)
    end;

maybe_convertable([{}], [{record, RecName} | Types], Options) ->
    try RecName:from_json([{}], Options) of
        Res ->
            Res
    catch
        error:undef ->
            maybe_convertable([{}], Types, Options)
    end;

maybe_convertable(Json, [{record, RecName} | Types], Options) when is_list(Json), length(Json) > 0 ->
    AllTuples = lists:all(fun(In) ->
        case In of
            {_,_} -> true;
            _ -> false
        end
    end, Json),
    if
        AllTuples ->
            try RecName:from_json(Json, Options) of
                Res ->
                    Res
            catch
                error:undef ->
                    maybe_convertable(Json, Types, Options)
            end;
        true ->
            maybe_convertable(Json, Types, Options)
    end;

maybe_convertable(Value, [{record, _RecName} | Types], Options) ->
    maybe_convertable(Value, Types, Options);

maybe_convertable(Values, [{list, ListTypes} | _Types], Options) when is_list(Values) ->
    Indexs = lists:seq(1, length(Values)),
    Indexed = lists:zip(Indexs, Values),
    {NewValues, Warnings} = lists:foldl(fun({Index, Value}, {ValAcc, WarnAcc}) ->
        case maybe_convertable(Value, ListTypes, Options) of
            error ->
                {ValAcc ++ [Value], WarnAcc ++ [Index]};
            {ok, NewVal} ->
                {ValAcc ++ [NewVal], WarnAcc};
            {ok, NewVal, NewWarns} ->
                {ValAcc ++ [NewVal], WarnAcc ++ [[Index | W] || W <- NewWarns]}
        end
    end, {[], []}, Indexed),
    case Warnings of
        [] ->
            {ok, NewValues};
        _ ->
            {ok, NewValues, Warnings}
    end;

maybe_convertable(Value, [{list, _ListTypes} | Types], Options) ->
    maybe_convertable(Value, Types, Options).

to_json_apply_transformations(_Tuple, [], []) ->
    [{}];
to_json_apply_transformations(_Typle, Json, []) ->
    Json;
to_json_apply_transformations(Tuple, Json, [{K, _V} = Prop | Tail]) ->
    Json2 = lists:keystore(K, 1, Json, Prop),
    to_json_apply_transformations(Tuple, Json2, Tail);

to_json_apply_transformations(Tuple, Json, [Atom | Tail]) when is_atom(Atom) ->
    Json2 = case lists:keytake(Atom, 1, Json) of
        false -> Json;
        {value, _, NewList} -> NewList
    end,
    to_json_apply_transformations(Tuple, Json2, Tail);

to_json_apply_transformations(Tuple, Json, [Fun | Tail]) when is_function(Fun) ->
    Json2 = case erlang:fun_info(Fun, arity) of
        {arity, 1} ->
            Fun(Json);
        {arity, 2} ->
            Fun(Json, Tuple);
        _ ->
            Json
    end,
    to_json_apply_transformations(Tuple, Json2, Tail).

extract_to_json_opts(Opts) ->
    extract_to_json_opts(Opts, skip, []).

extract_to_json_opts([], TreatUndef, Acc) ->
    {TreatUndef, lists:reverse(Acc)};

extract_to_json_opts([{null_is_undefined} | Tail], _TreatUndef, Acc) ->
    extract_to_json_opts(Tail, null, Acc);

extract_to_json_opts([Transform | Tail], TreatUndef, Acc) ->
    extract_to_json_opts(Tail, TreatUndef, [Transform | Acc]).

%% ---------------------------------------------------------------------------
%% verify types
%% ---------------------------------------------------------------------------

verify_type(null, [], any, TreatNull, _RecurseOpt) ->
    {ok, TreatNull};
verify_type(null, [], _Any, TreatNull, _RecurseOpt) ->
    {warn, TreatNull};
verify_type(Val, [], any, _TreatNull, _RecurseOpt) ->
    {ok, Val};
verify_type(Val, [], _Any, _TreatNull, _RecurseOpt) ->
    {warn, Val};
verify_type(null, [NullType | _Tail], _Any, NullType, _RecurseOpt) ->
    {ok, NullType};
%verify_type(Val, [{Type, []} | Tail], Any, Nulltype, RecurseOpt) ->
%    verify_type(Val, [Type | Tail], Any, Nulltype, RecurseOpt);
verify_type(Val, [{integer, []} | _Tail], _Any, _TreatNull, _Opt) when is_integer(Val) ->
    {ok, Val};
verify_type(Val, [{float, []} | _Tail], _Any, _TreatNull, _Opt) when is_float(Val) ->
    {ok, Val};
verify_type(true, [{boolean, []} | _Tail], _Any, _TreatNull, _Opt) ->
    {ok, true};
verify_type(false, [{boolean, []} | _Tail], _Any, _TreatNull, _Opt) ->
    {ok, false};
verify_type(Val, [{binary, []} | _Tail], _Any, _TreatNull, _Opt) when is_binary(Val) ->
    {ok, Val};
verify_type(Val, [{pos_integer, []} | _Tail], _Any, _TreatNull, _Opt) when is_integer(Val), Val > 0 ->
    {ok, Val};
verify_type(Val, [{non_neg_integer, []} | _Tail], _Any, _TreatNull, _Opt) when is_integer(Val), Val > -1 ->
    {ok, Val};
verify_type(Val, [{neg_integer, []} | _Tail], _Any, _TreatNull, _Opt) when is_integer(Val), Val < 0 ->
    {ok, Val};
verify_type(Val, [{number, []} | _Tail], _Any, _TreatNull, _Opt) when is_number(Val) ->
    {ok, Val};
verify_type(Val, [{float, []} | _Tail], _Any, _TreatNull, _Opt) when is_float(Val) ->
    {ok, Val};
verify_type([{}] = Json, [{record, [RecName]} | _Tail], _Any, _TreatNull, Opt) ->
    verify_type_record(Json, RecName, Opt);
verify_type([{_K, _V} | _OTail] = Val, [{record, [RecName]} | _Tail], _Any, _TreatNull, Opt) ->
    verify_type_record(Val, RecName, Opt);
verify_type(Vals, [{list, {ListAny, Types}} | _Tail], _Any, TreatNull, Opt) when is_list(Vals) ->
    verify_types(Vals, Types, ListAny, TreatNull, Opt);
verify_type(Val, [Atom | Tail], Any, TreatNull, Opt) when is_binary(Val), is_atom(Atom) ->
    case list_to_binary(atom_to_list(Atom)) of
        Val ->
            {ok, Atom};
        _ ->
            verify_type(Val, Tail, Any, TreatNull, Opt)
    end;
verify_type(Val, [{Module, Function, Args} | Tail], Any, TreatNull, Opt) ->
    case erlang:function_exported(Module, Function, length(Args) + 1) of
        true ->
            case erlang:apply(Module, Function, [Val | Args]) of
                true ->
                    {ok, Val};
                {ok, NewVal} ->
                    {ok, NewVal};
                false ->
                    {warn, Val}
            end;
        false ->
            verify_type(Val, Tail, Any, TreatNull, Opt)
    end;
verify_type(Val, [_Type | Tail], Any, TreatNull, Opt) ->
    %?log("unrecognized type ~p, evaling val ~p", [Type, Val]),
    verify_type(Val, Tail, Any, TreatNull, Opt).

verify_type_record(Json, RecName, Opt) ->
    case erlang:function_exported(RecName, from_json, 2) of
        true ->
            case RecName:from_json(Json, Opt) of
                {ok, Rec} ->
                    {ok, Rec};
                {ok, Rec, Warns} ->
                    {warn, Rec, Warns}
            end;
        false ->
            case code:ensure_loaded(RecName) of
                {error, _} ->
                    {warn, Json};
                {module, RecName} ->
                    verify_type_record(Json, RecName, Opt)
            end
    end.

verify_types(Vals, Types, Any, TreatNull, Opt) ->
    verify_types(Vals, Types, Any, TreatNull, Opt, 1, [], []).

verify_types([], _Types, _Any, _TreatNull, _Opt, _Index, WarnIndexes, Acc) ->
    Acc1 = lists:reverse(Acc),
    case WarnIndexes of
        [] ->
            {ok, Acc1};
        _ ->
            Indexes = lists:reverse(WarnIndexes),
            {warn, Acc1, Indexes}
    end;
verify_types([Val | Tail], Types, Any, TreatNull, Opt, Index, WarnInd, Acc) ->
    %?log("verifying val ~p against types ~p", [Val, Types]),
    case verify_type(Val, Types, Any, TreatNull, Opt) of
        {ok, Val1} ->
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, WarnInd, [Val1 | Acc]);
        {warn, Val1} ->
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, [Index | WarnInd], [Val1 | Acc]);
        {warn, Val1, Paths} when is_list(Paths) ->
            Paths1 = [[Index | Path] || Path <- Paths],
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, [Paths1 | WarnInd], [Val1 | Acc]);
        {warn, Val1, Path} ->
            Path1 = [Index, Path],
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, [Path1 | WarnInd], [Val1 | Acc])
    end.
