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

-export([parse_file/1,parse_file/2]).
-export([verify_type/5, verify_types/5]).
-export([to_json/3, to_json/4]).
-export([main/1]).

%% ---------------------------------------------------------------------------
%% escript file
%% ---------------------------------------------------------------------------

main(Args) ->
    case parse_opts(Args) of
        {error, _} -> help();
        {ok, Sources, Include, Outdir} ->
            compile_sources(Sources, Include, Outdir)
    end.

help() ->
    Str =
        "usage: rec2json -src=file_glob -dest=path/to/ebin -include=include\n"
        "    -src can be specified more than once.\n",
    io:format("~s", [Str]).

parse_opts(Opts) ->
    parse_opts(Opts, [], "include", ".").

parse_opts([], Src, Inc, Out) ->
    {ok, Src, Inc, Out};

parse_opts(["-src=" ++ SrcGlob | Tail], Srcs, Inc, Out) ->
    MoreSrcs = filelib:wildcard(SrcGlob),
    parse_opts(Tail, Srcs ++ MoreSrcs, Inc, Out);

parse_opts(["-dest=" ++ Dest | Tail], Srcs, Inc, _Out) ->
    parse_opts(Tail, Srcs, Inc, Dest);

parse_opts(["-include=" ++ Inc | Tail], Srcs, _Imp, Out) ->
    parse_opts(Tail, Srcs, Inc, Out);

parse_opts([What | _], _, _, _) ->
    {error, {badotp, What}}.

compile_sources([], _Inc, _Out) ->
    ok;

compile_sources([Src | Tail], Inc, Out) ->
    io:format("compiling ~s\n", [Src]),
    rec2json_compile:scan_file(Src, [{imports_dir, Inc}, {output_dir, Out}]),
    compile_sources(Tail, Inc, Out).

%% ---------------------------------------------------------------------------
%% to be used maybe someday?
%% ---------------------------------------------------------------------------

parse_file(FileName) ->
    parse_file(FileName, []).

parse_file(FileName, Options) ->
    {error, nyi}.

parse(String) ->
    parse(String, []).

parse(String, Opts) ->
    {error, nyi}.

%% ---------------------------------------------------------------------------
%% to json
%% ---------------------------------------------------------------------------

to_json(Tuple, FieldNames, Options) ->
    {TreatUndef, Transforms} = extract_to_json_opts(Options),
    to_json(Tuple, FieldNames, TreatUndef, Transforms).

to_json(Tuple, FieldNames, TreatUndef, Transforms) ->
    [_RecName | Values] = tuple_to_list(Tuple),
    Proplist = lists:zip(FieldNames, Values),
    ToTransforms = case to_json_props(Proplist, TreatUndef, Transforms) of
        [{}] -> [];
        JsonProp -> JsonProp
    end,
    to_json_apply_transformations(Tuple, ToTransforms, Transforms).

to_json_apply_transformations(_Tuple, [], []) ->
    [{}];
to_json_apply_transformations(_Typle, Json, []) ->
    Json;
to_json_apply_transformations(Tuple, Json, [{K, V} = Prop | Tail]) ->
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

to_json_value(undefined, skip, _Transforms) ->
    skip;
to_json_value(undefined, null, _Transforms) ->
    {ok, null};
to_json_value(Val, _TreatUndef, _Transforms) when is_atom(Val); is_binary(Val); is_integer(Val); is_float(Val) ->
    {ok, Val};
to_json_value(Tuple, TreatUndef, Transforms) when is_tuple(Tuple), is_atom(element(1, Tuple)) ->
    RecName = element(1, Tuple),
    case erlang:function_exported(RecName, to_json, 2) of
        false ->
            skip;
        true ->
            Trans = case TreatUndef of
                skip -> Transforms;
                null -> [{null_is_undefined} | Transforms]
            end,
            Json = RecName:to_json(Tuple, Trans),
            {ok, Json}
    end;
to_json_value(List, TreatUndef, Transforms) when is_list(List) ->
    PropTest = fun
        ({K,V}) when is_atom(K) orelse is_binary(K) -> true;
        (_) -> false
    end,
    case lists:all(PropTest, List) of
        true ->
            Val = to_json_props(List, TreatUndef, Transforms),
            {ok, Val};
        false ->
            Vals = to_json_values(List, TreatUndef, Transforms),
            {ok, Vals}
    end;

to_json_value(_Val, _TreatUndef, _Transforms) ->
    skip.

to_json_values(Vals, TreatUndef, Transforms) ->
    to_json_values(Vals, TreatUndef, Transforms, []).

to_json_values([], _TreatUndef, _Transforms, Acc) ->
    lists:reverse(Acc);

to_json_values([Val | Tail], TreatUndef, Transforms, Acc) ->
    case to_json_value(Val, TreatUndef, Transforms) of
        skip ->
            to_json_values(Tail, TreatUndef, Transforms);
        {ok, Val2} ->
            to_json_values(Tail, TreatUndef, Transforms, [Val2 | Acc])
    end.

to_json_props(Props, TreatUndef, Transforms) ->
    to_json_props(Props, TreatUndef, Transforms, []).

to_json_props([], _TreatUndef, _Transforms, []) ->
    [{}];
to_json_props([], _TreatUndef, _Transform, Acc) ->
    lists:reverse(Acc);
to_json_props([{Key, Val} | Tail], TreatUndef, Transform, Acc) ->
    case to_json_value(Val, TreatUndef, Transform) of
        skip ->
            to_json_props(Tail, TreatUndef, Transform, Acc);
        {ok, Val2} ->
            to_json_props(Tail, TreatUndef, Transform, [{Key, Val2} | Acc])
    end.

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
verify_type(Val, [Type | Tail], Any, TreatNull, Opt) ->
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
