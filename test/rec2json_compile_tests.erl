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
        ?debugFmt("ka burble! ~p", [file:get_cwd()]),
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
        end},

        {"from json with type:  pos_integer", fun() ->
            ?assert(triq:check(prop_pos_integer()))
        end},

        {"from json with type:  int_or_bool", fun() ->
            ?assert(triq:check(prop_int_or_bool()))
        end},

        {"from json with type:  combo_type", fun() ->
            ?assert(triq:check(prop_combo_type()))
        end},

        {"from json binary to atom conversion", fun() ->
            ?assert(triq:check(prop_atoms()))
        end}

    ] end}.

%% triq funcs.
prop_pos_integer() ->
    ?FORALL(Int, int(),
    begin
        Expected = #feature{over_zero = Int},
        Json = [{over_zero, Int}],
        Got = feature:from_json(Json),
        if
            Int > 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [over_zero]} == Got
        end
    end).

prop_int_or_bool() ->
    ?FORALL(IntOrBool, oneof([int(), bool(), binary(), real()]),
    begin
        Expected = #feature{int_or_bool = IntOrBool},
        Json = [{int_or_bool, IntOrBool}],
        Got = feature:from_json(Json),
        case IntOrBool of
            X when is_integer(X); is_boolean(X) ->
                {ok, Expected} == Got;
            _ ->
                {ok, Expected, [int_or_bool]} == Got
        end
    end).

prop_combo_type() ->
    ?FORALL(IntOrBool, oneof([int(), bool(), binary()]),
    begin
        Expected = #feature{combo_type = IntOrBool},
        Json = [{combo_type, IntOrBool}],
        Got = feature:from_json(Json),
        if
            is_binary(IntOrBool) ->
                {ok, Expected, [combo_type]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_atoms() ->
    ?FORALL(Atom, oneof([init, ready, steady, go, stop, hop]),
    begin
        Json = [{atoms, list_to_binary(atom_to_list(Atom))}],
        Got = feature:from_json(Json),
        case lists:member(Atom, [init, ready, steady]) of
            true ->
                Expected = #feature{atoms = Atom},
                {ok, Expected} == Got;
            false ->
                Expected = #feature{atoms = list_to_binary(atom_to_list(Atom))},
                {ok, Expected, [atoms]} == Got
        end
    end).
