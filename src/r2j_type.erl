%% @doc Module which holds and exports the custome types and validators
%% which will ship with rec2json.
-module(r2j_type).

-export([integer/1, pos_integer/1, non_neg_integer/1, neg_integer/1, float/1,
    number/1, boolean/1, binary/1, unsafe_atom/1]).
-export([integer/3, atom/2]).
-export([string/2]).

-type unsafe_atom() :: atom().
-export_types([
    unsafe_atom/0
]).


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
float(N) when is_integer(N) ->
    try N * 1.0 of
        _ ->
            {ok, N}
    catch
      {error, badarith} ->
          error
    end;
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

%% @doc Translation for any atom at all. This is marked as unsafe because if
%% the json to decode is taken from an untrusted source, and you use this type
%% on a field, that untrusted source could fill up your atom table. Atoms are
%% not garbage collected, ever, so the untrusted source could also fill up your
%% systems memory. Even a trusted source can go wonky. As such, think long and
%% hard before you go ahead an use this.
unsafe_atom(Atom) when is_atom(Atom) ->
    {ok, atom_to_binary(Atom, utf8)};

unsafe_atom(Binary) when is_binary(Binary) ->
    {ok, binary_to_atom(Binary, utf8)};

unsafe_atom(_Else) ->
    error.
