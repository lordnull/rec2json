%% @doc Module which holds and exports the custome types and validators
%% which will ship with rec2json.
-module(r2j_type).

-export([integer/1, pos_integer/1, non_neg_integer/1, neg_integer/1, float/1,
    number/1, boolean/1, binary/1]).
-export([integer/3, atom/2]).
-export([string/2]).


integer(N) when is_integer(N) ->
    {ok, N};

integer(_N) ->
    error.

pos_integer(N) when is_integer(N), 0 < N ->
    {ok, N};
pos_integer(_N) ->
    error.

non_neg_integer(N) when is_integer(N), 0 =< N ->
    {ok, N};
non_neg_integer(_) ->
    error.

neg_integer(N) when is_integer(N), N < 0 ->
    {ok, N};
neg_integer(_) ->
    error.

float(N) when is_float(N) ->
    {ok, N};
float(_) ->
    error.

number(N) when is_number(N) ->
    {ok, N};
number(_) ->
    error.

boolean(N) when N; not N ->
    {ok, N};
boolean(_) ->
    error.

binary(N) when is_binary(N) ->
    {ok, N};
binary(_) ->
    error.

integer(N, Min, Max) when is_integer(N), Min =< N, N =< Max ->
    {ok, N};
integer(_,_,_) ->
    error.

atom(Bin, Atom) when is_binary(Bin) ->
    case list_to_binary(atom_to_list(Atom)) of
        Bin ->
            {ok, Atom};
        _ ->
            error
    end;

atom(Atom, Atom) when is_atom(Atom) ->
    {ok, list_to_binary(atom_to_list(Atom))};

atom(_Atom, _OtherAtom) ->
    error.

string(Str, Len) when is_binary(Str), size(Str) =< Len ->
    {ok, Str};
string(_Str, _Len) ->
    error.

