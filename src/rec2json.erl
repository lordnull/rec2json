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

-export([parse_file/1,parse_file/2]).
-export([verify_type/5, verify_types/5]).

parse_file(FileName) ->
    parse_file(FileName, []).

parse_file(FileName, Options) ->
    {error, nyi}.

parse(String) ->
    parse(String, []).

parse(String, Opts) ->
    {error, nyi}.

verify_type(Val, [], any, _TreatNull, _RecurseOpt) ->
    {ok, Val};
verify_type(Val, [], _Any, _TreatNull, _RecurseOpt) ->
    {warn, Val};
verify_type(null, [NullType | _Tail], _Any, Nulltype, _RecurseOpt) ->
    {ok, NullType};
verify_type(Val, [integer | _Tail], _Any, _TreatNull, _Opt) when is_integer(Val) ->
    {ok, Val};
verify_type(Val, [float | _Tail], _Any, _TreatNull, _Opt) when is_float(Val) ->
    {ok, Val};
verify_type(true, [boolean | _Tail], _Any, _TreatNull, _Opt) ->
    {ok, true};
verify_type(false, [boolean | _Tail], _Any, _TreatNull, _Opt) ->
    {ok, false};
verify_type(Val, [binary | _Tail], _Any, _TreatNull, _Opt) when is_binary(Val) ->
    {ok, Val};
verify_type([{}], [{record, RecName} | _Tail], _Any, _TreatNull, Opt) ->
    case erlang:function_exported(RecName, from_json, 1) of
        true ->
            case RecName:from_json([{}], Opt) of
                {ok, Rec} ->
                    {ok, Rec};
                {ok, Rec, Warns} ->
                    {warn, Rec, Warns}
            end;
        false ->
            {warn, [{}]}
    end;
verify_type([{_K, _V} | _OTail] = Val, [{record, RecName} | _Tail], _Any, _TreatNull, Opt) ->
    case erlang:function_exported(RecName, from_json, 1) of
        true ->
            case RecName:from_json(Val, Opt) of
                {ok, Rec} ->
                    {ok, Rec};
                {ok, Rec, Warns} ->
                    {warn, Rec, Warns}
            end;
        false ->
            {warn, Val}
    end;
verify_type(Vals, [{list, Types} | _Tail], Any, TreatNull, Opt) ->
    verify_types(Vals, Types, Any, TreatNull, Opt);
verify_type(Val, [_Type | Tail], Any, TreatNull, Opt) ->
    verify_type(Val, Tail, Any, TreatNull, Opt).

verify_types(Vals, Types, Any, TreatNull, Opt) ->
    verify_types(Vals, Types, Any, TreatNull, Opt, 0, [], []).

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
    case verify_type(Val, Types, Any, TreatNull, Opt) of
        {ok, Val1} ->
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, WarnInd, [Val1 | Acc]);
        {warn, Val1} ->
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, [Index | WarnInd], [Val1 | Acc]);
        {warn, Val1, Paths} ->
            Paths1 = [[Index | Path] || Path <- Paths],
            verify_types(Tail, Types, Any, TreatNull, Opt, Index + 1, [Paths1 | WarnInd], [Val1 | Acc])
    end.
