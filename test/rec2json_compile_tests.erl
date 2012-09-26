-module(rec2json_compile_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-include("test/rec2json_compile_tests.hrl").

-record(basic_rec, {
    boolean_thing,
    unicode_str,
    count,
    maybe_count
}).

feature_test_() ->
    {setup, fun() ->
        rec2json_compile:scan_file("../test/rec2json_compile_tests.hrl", [])
    end, fun(_) ->
        ok
    end, fun(_) -> [

        {"to json as parameterized module", fun() ->
            Record = #included{},
            ?assertEqual([{}], Record:to_json())
        end},

        {"to json as regular module", fun() ->
            Record = #included{},
            ?assertEqual([{}], included:to_json(Record))
        end},

        {"to json as parametrized module, undefined is null", fun() ->
            Record = #included{},
            ?assertEqual([{field, null}], Record:to_json([{null_is_undefined}]))
        end},

        {"To json as regular module, undefined is null", fun() ->
            Record = #included{},
            ?assertEqual([{field, null}], included:to_json(Record, [{null_is_undefined}]))
        end},

        {"To json with adding prop mutator", fun() ->
            Record = #included{},
            Expected = [{newage, <<"hi">>}],
            ?assertEqual(Expected, included:to_json(Record, [{newage, <<"hi">>}]))
        end},

        {"To json with removing prop mutator", fun() ->
            Record = #included{field = <<"hi">>},
            Expected = [{}],
            ?assertEqual(Expected, included:to_json(Record, [field]))
        end},

        {"To json with arity 1 fun mutator", fun() ->
            Mutator = fun(J) ->
                ?assertEqual([], J),
                []
            end,
            Expected = [{}],
            Record = #included{},
            ?assertEqual(Expected, included:to_json(Record, [Mutator]))
        end},

        {"To json with arity 2 fun mutator", fun() ->
            Record = #included{},
            Mutator = fun(J, R) ->
                ?assertEqual([], J),
                ?assertEqual(Record, R),
                []
            end,
            ?assertEqual([{}], included:to_json(Record, [Mutator]))
        end},

        {"To json with all mutators", fun() ->
            Record = #included{field = 3},
            A1 = fun(J) ->
                ?assertEqual([{new1, <<"hi">>}], J),
                [{mut1, 1} | J]
            end,
            A2 = fun(J, R) ->
                ?assertEqual(Record, R),
                [{mut2, 2} | J]
            end,
            Mutators = [field, {new1, <<"hi">>}, A1, A2],
            Expected = [{mut2, 2}, {mut1, 1}, {new1, <<"hi">>}],
            ?assertEqual(Expected, included:to_json(Record, Mutators))
        end},

        {"To json, sub record encoded as empty obj", fun() ->
            IncRec = #included{},
            Record = #feature{default = undefined, default_integer = undefined, record_type = IncRec},
            ?assertEqual([{record_type, [{}]}], Record:to_json())
        end},

        {"To json, sub record fully encoded", fun() ->
            IncRec = #included{field = <<"field">>},
            Record = #feature{default = undefined, default_integer = undefined, record_type = IncRec},
            ?assertEqual([{record_type, [{field, <<"field">>}]}], Record:to_json())
        end},

        {"from json with binary fields", fun() ->
            Expected = #included{field = <<"field">>},
            ?assertEqual({ok, Expected}, included:from_json([{<<"field">>, <<"field">>}]))
        end},

        {"from json with atom field", fun() ->
            Expected = #included{field = <<"field">>},
            ?assertEqual({ok, Expected}, included:from_json([{field, <<"field">>}]))
        end},

        {"from empty json object", fun() ->
            Expected = #included{},
            ?assertEqual({ok, Expected}, included:from_json([{}]))
        end},

        {"from json object with unspecified field", fun() ->
            Expected = #included{},
            Json = [{thang, 71}],
            ?assertEqual({ok, Expected}, included:from_json(Json))
        end},

        {"from json with seed record", fun() ->
            Seed = #included{field = 32},
            ?assertEqual({ok, Seed}, included:from_json(Seed, [{}]))
        end},

        {"from json with seed record as parameterized module", fun() ->
            Seed = #included{field = 32},
            ?assertEqual({ok, Seed}, Seed:from_json([{}]))
        end},

        {"from json with null as null", fun() ->
            Expected = #included{field = null},
            Json = [{field, null}],
            ?assertEqual({ok, Expected}, included:from_json(Json))
        end},

        {"from json with null as undefined", fun() ->
            Expected = #included{field = undefined},
            Json = [{field, null}],
            ?assertEqual({ok, Expected}, included:from_json(Json, [null_is_undefined]))
        end},

        {"from json with included record", fun() ->
            Expected = #feature{record_type = #included{}},
            Json = [{record_type, [{}]}],
            ?assertEqual({ok, Expected}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  integer", fun() ->
            Expected = #feature{integer_type = <<"hi">>},
            Json = [{integer_type, <<"hi">>}],
            ?assertEqual({ok, Expected, [integer_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  boolean", fun() ->
            Expected = #feature{boolean_type = 42},
            Json = [{boolean_type, 42}],
            ?assertEqual({ok, Expected, [boolean_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  binary", fun() ->
            Expected = #feature{binary_type = false},
            Json = [{binary_type, false}],
            ?assertEqual({ok, Expected, [binary_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch: list", fun() ->
            Expected = #feature{list_type = null},
            Json = [{list_type, null}],
            Got = feature:from_json(Json),
            ?assertEqual({ok, Expected, [list_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch: list 2", fun() ->
            Expected = #feature{list_type = [<<"hi">>]},
            Json = [{list_type, [<<"hi">>]}],
            ?assertEqual({ok, Expected, [[list_type, 1]]}, feature:from_json(Json))
        end},

        {"from json with type mismatch: null", fun() ->
            Expected = #feature{null_type = <<"hi">>},
            Json = [{null_type, <<"hi">>}],
            ?assertEqual({ok, Expected, [null_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  record_type", fun() ->
            Expected = #feature{record_type = 33},
            Json = [{record_type, 33}],
            ?assertEqual({ok, Expected, [record_type]}, feature:from_json(Json))
        end}

    ] end}.

triq_test_() ->
    Exported = ?MODULE:module_info(exports),
    Exported1 = lists:filter(fun
        ({Atom, 0}) ->
            case atom_to_list(Atom) of
                "prop_" ++ _ -> true;
                _ -> false
            end;
        (_) ->
            false
    end, Exported),
    Exported2 = [F || {F, _} <- Exported1],
    triq_test_gen(Exported2).

triq_test_gen([]) ->
    {generator, fun() -> [] end};
triq_test_gen([TriqTest | Tail]) ->
    {generator, fun() -> [
        {atom_to_list(TriqTest), fun() ->
            ?assert(triq:check(erlang:apply(?MODULE, TriqTest, []), 100))
        end} |
        triq_test_gen(Tail) ]
    end}.

%% triq funcs.
prop_integer() ->
    rec2json_compile:scan_string("-record(prop_integer, {f :: integer()}).", []),
    ?FORALL(Val, oneof([int(), real()]),
    begin
        Expected = {prop_integer, Val},
        Json = [{f, Val}],
        Got = prop_integer:from_json(Json),
        if
            is_integer(Val) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_pos_integer() ->
    rec2json_compile:scan_string("-record(prop_pos_integer, {f :: pos_integer()}).", []),
    ?FORALL(Int, int(),
    begin
        Expected = {prop_pos_integer, Int},
        Json = [{f, Int}],
        Got = prop_pos_integer:from_json(Json),
        if
            Int > 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_int_or_bool() ->
    rec2json_compile:scan_string("-record(prop_int_or_bool, {f :: integer() | boolean()}).", []),
    ?FORALL(IntOrBool, oneof([int(), bool(), binary(), real()]),
    begin
        Expected = {prop_int_or_bool, IntOrBool},
        Json = [{f, IntOrBool}],
        Got = prop_int_or_bool:from_json(Json),
        case IntOrBool of
            X when is_integer(X); is_boolean(X) ->
                {ok, Expected} == Got;
            _ ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_atoms() ->
    rec2json_compile:scan_string("-record(prop_atoms, {f :: 'init' | 'ready' | 'steady'}).", []),
    ?FORALL(Atom, oneof([init, ready, steady, go, stop, hop]),
    begin
        Json = [{f, list_to_binary(atom_to_list(Atom))}],
        Got = prop_atoms:from_json(Json),
        case lists:member(Atom, [init, ready, steady]) of
            true ->
                Expected = {prop_atoms, Atom},
                {ok, Expected} == Got;
            false ->
                Expected = {prop_atoms, list_to_binary(atom_to_list(Atom))},
                {ok, Expected, [f]} == Got
        end
    end).

prop_non_neg_integer() ->
    rec2json_compile:scan_string("-record(prop_non_neg_integer, {f :: non_neg_integer()}).", []),
    ?FORALL(Int, int(),
    begin
        Json = [{f, Int}],
        Got = prop_non_neg_integer:from_json(Json),
        Expected = {prop_non_neg_integer, Int},
        if
            Int < 0 ->
                {ok, Expected, [f]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_boolean() ->
    rec2json_compile:scan_string("-record(prop_boolean, {f :: boolean()}).", []),
    ?FORALL(Bool, oneof([bool(), goober]),
    begin
        Expected = {prop_boolean, Bool},
        Json = [{f, Bool}],
        Got = prop_boolean:from_json(Json),
        if
            Bool == goober ->
                {ok, Expected,[f]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_neg_integer() ->
    rec2json_compile:scan_string("-record(prop_neg_integer, {f :: neg_integer()}).", []),
    ?FORALL(Int, int(),
    begin
        Expected = {prop_neg_integer, Int},
        Json = [{f, Int}],
        Got = prop_neg_integer:from_json(Json),
        if
            Int < 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_number() ->
    rec2json_compile:scan_string("-record(prop_number, {f :: number()}).", []),
    ?FORALL(Number, oneof([int(), real(), goober]),
    begin
        Expected = {prop_number, Number},
        Json = [{f, Number}],
        Got = prop_number:from_json(Json),
        if
            is_number(Number) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_binary() ->
    rec2json_compile:scan_string("-record(prop_binary, {f :: binary()}).", []),
    ?FORALL(Binary, oneof([list(int()), binary()]),
    begin
        Expected = {prop_binary, Binary},
        Json = [{f, Binary}],
        Got = prop_binary:from_json(Json),
        if
            is_binary(Binary) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_float() ->
    rec2json_compile:scan_string("-record(prop_float, {f :: float()}).", []),
    ?FORALL(Float, oneof([int(), real()]),
    begin
        Expected = {prop_float, Float},
        Json = [{f, Float}],
        Got = prop_float:from_json(Json),
        if
            is_float(Float) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_list_one() ->
    rec2json_compile:scan_string("-record(prop_list_one, {f :: [integer()]}).", []),
    ?FORALL(List, list(oneof([int(), real()])),
    begin
        Expected = {prop_list_one, List},
        Json = [{f, List}],
        Got = prop_list_one:from_json(Json),
        WarnsFun = fun
            (Item, _Ind, Acc) when is_integer(Item) ->
                Acc;
            (_Item, Ind, Acc) ->
                [[f, Ind] | Acc]
        end,
        case fold_ind(WarnsFun, [], List) of
            [] ->
                {ok, Expected} == Got;
            Warns ->
                Warns1 = lists:reverse(Warns),
                {ok, Expected, Warns1} == Got
        end
    end).

fold_ind(Fun, Acc, List) ->
    fold_ind(Fun, Acc, 1, List).

fold_ind(_Fun, Acc, _Ind, []) ->
    Acc;
fold_ind(Fun, Acc, Ind, [Item | Tail]) ->
    Acc2 = Fun(Item, Ind, Acc),
    fold_ind(Fun, Acc2, Ind + 1, Tail).
