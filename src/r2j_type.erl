%% @doc Module which holds and exports the custome types and validators
%% which will ship with rec2json.
-module(r2j_type).

-opaque integer() :: integer().
-opaque integer(Min, Max) :: integer(Min, Max).

-export_type([integer/0]).
-export_type([integer/2]).

-export([integer/1]).
-export([integer/3]).


integer(N) ->
    is_integer(N).

integer(N, Min, Max) when is_integer(N), Min =< N, N =< Max ->
    true;
integer(_,_,_) ->
    false.

