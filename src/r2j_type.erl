%% @doc Module which holds and exports the custome types and validators
%% which will ship with rec2json.
-module(r2j_type).

-opaque integer() :: integer().

-export_type([integer/0]).
-export([integer/1]).


integer(N) ->
    is_integer(N).

