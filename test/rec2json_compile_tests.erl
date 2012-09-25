-module(rec2json_compile_tests).

-include_lib("eunit/include/eunit.hrl").
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
            ?assertEqual({ok, Expected, [list_type]}, feature:from_json(Json))
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

%basic_compiled_module_test_() ->
%    {setup, fun() ->
%        RecDefinition = "-record(basic_rec, { boolean_thing, unicode_str, count, maybe_count }).",
%        rec2json_compile:scan_string(RecDefinition, [])
%    end, fun(_) ->
%        ok
%    end, fun(_) -> [
%
%        {"From json with binary labels", fun() ->
%            Json = [
%                {<<"boolean_thing">>, true}, {<<"unicode_str">>, <<"hi boss">>},
%                {<<"count">>, 37}, {<<"maybe_count">>, null}
%            ],
%            Expected = #basic_rec{boolean_thing = true, unicode_str = <<"hi boss">>, count = 37, maybe_count = null},
%            ?assertEqual({ok, Expected}, basic_rec:from_json(Json))
%        end},
%
%        {"From json with atom labels", fun() ->
%            Json = [
%                {boolean_thing, true}, {unicode_str, <<"hi boss">>},
%                {count, 37}, {maybe_count, null}
%            ],
%            Expected = #basic_rec{boolean_thing = true, unicode_str = <<"hi boss">>, count = 37, maybe_count = null},
%            ?assertEqual({ok, Expected}, basic_rec:from_json(Json))
%        end},
%
%        {"From json, nulls converted to undefined", fun() ->
%            Json = [
%                {boolean_thing, true}, {unicode_str, null},
%                {count, 37}, {maybe_count, null}
%            ],
%            Expected = #basic_rec{boolean_thing = true, unicode_str = undefined, count = 37, maybe_count = undefined},
%            ?assertEqual({ok, Expected}, basic_rec:from_json(Json, [null_is_undefined]))
%        end},
%
%        {"To json, undefined skipped", fun() ->
%            Expected = [
%                {boolean_thing, true}, {maybe_count, null}
%            ],
%            Rec = #basic_rec{boolean_thing = true, unicode_str = undefined, count = undefined, maybe_count = null},
%            ?assertEqual(Expected, basic_rec:to_json(Rec))
%        end},
%
%        {"To json, undefined to null", fun() ->
%            Expected = [
%                {boolean_thing, true}, {unicode_str, null},
%                {count, 37}, {maybe_count, null}
%            ],
%            Rec = #basic_rec{boolean_thing = true, unicode_str = undefined, count = 37, maybe_count = undefined},
%            ?assertEqual(Expected, basic_rec:to_json(Rec, [{null_is_undefined}]))
%        end},
%
%        {"To json, appending key/values", fun() ->
%            Expected = [
%                {boolean_thing, true}, {unicode_str, <<"hi friend">>},
%                {count, 37}, {maybe_count, null}, {goober, <<"pants">>}, {<<"id">>, 7}
%            ],
%            Rec = #basic_rec{boolean_thing = true, unicode_str = <<"hi friend">>, count = 37, maybe_count = null},
%            ?assertEqual(Expected, basic_rec:to_json(Rec, [{goober, <<"pants">>}, {<<"id">>, 7}]))
%        end},
%
%        {"To json, keys to remove", fun() ->
%            Expected = [
%                {unicode_str, null}, {maybe_count, null}
%            ],
%            Rec = #basic_rec{boolean_thing = true, unicode_str = undefined, count = 37, maybe_count = undefined},
%            ?assertEqual(Expected, basic_rec:to_json(Rec, [boolean_thing, count, {null_is_undefined}]))
%        end},
%
%        {"To json, all keys removed", fun() ->
%            Expected = [{}],
%            Rec = #basic_rec{boolean_thing = true, unicode_str = <<"yo">>, count = 37, maybe_count = null},
%            ?assertEqual(Expected, basic_rec:to_json(Rec, [maybe_count, boolean_thing, unicode_str, count, {null_is_undefined}]))
%        end},
%
%        {"To json, functions to alter json", fun() ->
%            Fun1 = fun(InJson) ->
%                proplists:delete(boolean_thing, InJson)
%            end,
%            Fun2 = fun(InJson) ->
%                [{propkey, false} | InJson]
%            end,
%            Expected = [
%                {propkey, false},{unicode_str, <<"hi friend">>},
%                {count, 42},{maybe_count, 42}
%            ],
%            Rec = #basic_rec{boolean_thing = false, unicode_str = <<"hi friend">>,
%                count = 42, maybe_count = 42},
%            ?assertEqual(Expected, basic_rec:to_json(Rec, [Fun1, Fun2]))
%        end},
%
%        {"To json, functions return empty list", fun() ->
%            Fun1 = fun(_) ->
%                []
%            end,
%            Expected = [{}],
%            Rec = #basic_rec{boolean_thing = false, unicode_str = <<"hi friend">>,
%                count = 42, maybe_count = 42},
%            ?assertEqual(Expected, basic_rec:to_json(Rec, [Fun1]))
%        end}
%
%    ] end}.
%
%-record(typed_rec, {
%    bool_or_undef :: boolean(),
%    bool = false :: boolean(),
%    int_or_undef :: integer(),
%    int = 0 :: integer()
%}).
%
%typed_compile_module_test_() ->
%    {setup, fun() ->
%        RecDefinition =
%            "-record(typed_rec, {"
%            "   bool_or_undef :: boolean(),"
%            "   bool = false :: boolean(),"
%            "   int_or_undef :: integer(),"
%            "   int = 0 :: integer()"
%            "}).",
%        rec2json_compile:scan_string(RecDefinition, [])
%    end, fun(_) ->
%        ok
%    end, fun(_) -> [
%
%        {"From json with binary labels", fun() ->
%            Json = [
%                {<<"bool_or_undef">>, false}, {<<"bool">>, true},
%                {<<"int_or_undef">>, 37}, {<<"int">>, 27}
%            ],
%            Expected = #typed_rec{bool_or_undef = false, bool = true, int_or_undef = 37, int = 27},
%            ?assertEqual({ok, Expected}, typed_rec:from_json(Json))
%        end},
%
%        {"From json with atom labels", fun() ->
%            Json = [
%                {<<"bool_or_undef">>, false}, {<<"bool">>, true},
%                {<<"int_or_undef">>, 37}, {<<"int">>, 27}
%            ],
%            Expected = #typed_rec{bool_or_undef = false, bool = true, int_or_undef = 37, int = 27},
%            ?assertEqual({ok, Expected}, typed_rec:from_json(Json))
%        end},
%
%        {"From json, nulls converted to undefined", fun() ->
%            Json = [
%                {bool_or_undef, null}, {<<"bool">>, true},
%                {<<"int_or_undef">>, null}, {int, 27}
%            ],
%            Expected = #typed_rec{bool_or_undef = undefined, bool = true, int_or_undef = undefined, int = 27},
%            ?assertEqual({ok, Expected}, typed_rec:from_json(Json, [null_is_undefined]))
%        end},
%
%        {"From json, type mismatchs gives warning", fun() ->
%            Json = [
%                {bool_or_undef, <<"string">>}, {<<"bool">>, 27}, {int_or_undef, true}, {int, null}
%            ],
%            Expected = #typed_rec{bool_or_undef = <<"string">>, bool = 27, int_or_undef = true, int = null},
%            Out = typed_rec:from_json(Json),
%            ?assertMatch({ok, Expected, _WarnList}, Out),
%            {ok, _, WarnList} = Out,
%            ExpectedWarnings = [bool_or_undef, bool, int_or_undef, int],
%            [?assert(lists:member(W, ExpectedWarnings)) || W <- WarnList]
%        end},
%
%        {"To json, undefined skipped", fun() ->
%            Expected = [
%                {bool, false},{int, 0}
%            ],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec))
%        end},
%
%        {"To json, undefined to null", fun() ->
%            Expected = [
%                {bool_or_undef, null}, {bool, false},
%                {int_or_undef, null}, {int, 0}
%            ],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec, [{null_is_undefined}]))
%        end},
%
%        {"To json, appending key/values", fun() ->
%            Expected = [
%                {bool, false}, {int, 0}, {goober, <<"pants">>}, {<<"id">>, 7}
%            ],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec, [{goober, <<"pants">>}, {<<"id">>, 7}]))
%        end},
%
%        {"To json, keys to remove", fun() ->
%            Expected = [
%                {int, 0}
%            ],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec, [bool]))
%        end},
%
%        {"To json, all keys removed", fun() ->
%            Expected = [{}],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec, [bool, int]))
%        end},
%
%        {"To json, functions to alter json", fun() ->
%            Fun1 = fun(InJson) ->
%                proplists:delete(bool, InJson)
%            end,
%            Fun2 = fun(InJson) ->
%                [{propkey, false} | InJson]
%            end,
%            Expected = [
%                {propkey, false},{int, 0}
%            ],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec, [Fun1, Fun2]))
%        end},
%
%        {"To json, functions return empty list", fun() ->
%            Fun1 = fun(_) ->
%                []
%            end,
%            Expected = [{}],
%            Rec = #typed_rec{},
%            ?assertEqual(Expected, typed_rec:to_json(Rec, [Fun1]))
%        end}
%
%    ] end}.
