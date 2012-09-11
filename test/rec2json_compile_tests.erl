-module(rec2json_compile_tests).

-include_lib("eunit/include/eunit.hrl").

-record(basic_rec, {
    boolean_thing,
    unicode_str,
    count,
    maybe_count
}).

basic_compiled_module_test_() ->
    {setup, fun() ->
        RecDefinition = "-record(basic_rec, { boolean_thing, unicode_str, count, maybe_count }).",
        rec2json_compile:scan_string(RecDefinition, [])
    end, fun(_) ->
        ok
    end, fun(_) -> [

        {"From json with binary labels", fun() ->
            Json = [
                {<<"boolean_thing">>, true}, {<<"unicode_str">>, <<"hi boss">>},
                {<<"count">>, 37}, {<<"maybe_count">>, null}
            ],
            Expected = #basic_rec{boolean_thing = true, unicode_str = <<"hi boss">>, count = 37, maybe_count = null},
            ?assertEqual({ok, Expected}, basic_rec:from_json(Json))
        end},

        {"From json with atom labels", fun() ->
            Json = [
                {boolean_thing, true}, {unicode_str, <<"hi boss">>},
                {count, 37}, {maybe_count, null}
            ],
            Expected = #basic_rec{boolean_thing = true, unicode_str = <<"hi boss">>, count = 37, maybe_count = null},
            ?assertEqual({ok, Expected}, basic_rec:from_json(Json))
        end},

        {"From json, nulls converted to undefined", fun() ->
            Json = [
                {boolean_thing, true}, {unicode_str, null},
                {count, 37}, {maybe_count, null}
            ],
            Expected = #basic_rec{boolean_thing = true, unicode_str = undefined, count = 37, maybe_count = undefined},
            ?assertEqual({ok, Expected}, basic_rec:from_json(Json, [null_is_undefined]))
        end},

        {"To json, undefined skipped", fun() ->
            Expected = [
                {boolean_thing, true}, {maybe_count, null}
            ],
            Rec = #basic_rec{boolean_thing = true, unicode_str = undefined, count = undefined, maybe_count = null},
            ?assertEqual(Expected, basic_rec:to_json(Rec))
        end},

        {"To json, undefined to null", fun() ->
            Expected = [
                {boolean_thing, true}, {unicode_str, null},
                {count, 37}, {maybe_count, null}
            ],
            Rec = #basic_rec{boolean_thing = true, unicode_str = undefined, count = 37, maybe_count = undefined},
            ?assertEqual(Expected, basic_rec:to_json(Rec, [{null_is_undefined}]))
        end},

        {"To json, appending key/values", fun() ->
            Expected = [
                {boolean_thing, true}, {unicode_str, <<"hi friend">>},
                {count, 37}, {maybe_count, null}, {goober, <<"pants">>}, {<<"id">>, 7}
            ],
            Rec = #basic_rec{boolean_thing = true, unicode_str = <<"hi friend">>, count = 37, maybe_count = null},
            ?assertEqual(Expected, basic_rec:to_json(Rec, [{goober, <<"pants">>}, {<<"id">>, 7}]))
        end},

        {"To json, keys to remove", fun() ->
            Expected = [
                {unicode_str, null}, {maybe_count, null}
            ],
            Rec = #basic_rec{boolean_thing = true, unicode_str = undefined, count = 37, maybe_count = undefined},
            ?assertEqual(Expected, basic_rec:to_json(Rec, [boolean_thing, count, {null_is_undefined}]))
        end},

        {"To json, all keys removed", fun() ->
            Expected = [{}],
            Rec = #basic_rec{boolean_thing = true, unicode_str = <<"yo">>, count = 37, maybe_count = null},
            ?assertEqual(Expected, basic_rec:to_json(Rec, [maybe_count, boolean_thing, unicode_str, count, {null_is_undefined}]))
        end},

        {"To json, functions to alter json", fun() ->
            Fun1 = fun(InJson) ->
                proplists:delete(boolean_thing, InJson)
            end,
            Fun2 = fun(InJson) ->
                [{propkey, false} | InJson]
            end,
            Expected = [
                {propkey, false},{unicode_str, <<"hi friend">>},
                {count, 42},{maybe_count, 42}
            ],
            Rec = #basic_rec{boolean_thing = false, unicode_str = <<"hi friend">>,
                count = 42, maybe_count = 42},
            ?assertEqual(Expected, basic_rec:to_json(Rec, [Fun1, Fun2]))
        end},

        {"To json, functions return empty list", fun() ->
            Fun1 = fun(_) ->
                []
            end,
            Expected = [{}],
            Rec = #basic_rec{boolean_thing = false, unicode_str = <<"hi friend">>,
                count = 42, maybe_count = 42},
            ?assertEqual(Expected, basic_rec:to_json(Rec, [Fun1]))
        end}

    ] end}.

-record(typed_rec, {
    bool_or_undef :: boolean(),
    bool = false :: boolean(),
    int_or_undef :: integer(),
    int = 0 :: integer()
}).

typed_compile_module_test_() ->
    {setup, fun() ->
        RecDefinition =
            "-record(typed_rec, {"
            "   bool_or_undef :: boolean(),"
            "   bool = false :: boolean(),"
            "   int_or_undef :: integer(),"
            "   int = 0 :: integer()"
            "}).",
        rec2json_compile:scan_string(RecDefinition, [])
    end, fun(_) ->
        ok
    end, fun(_) -> [

        {"From json with binary labels", fun() ->
            Json = [
                {<<"bool_or_undef">>, false}, {<<"bool">>, true},
                {<<"int_or_undef">>, 37}, {<<"int">>, 27}
            ],
            Expected = #typed_rec{bool_or_undef = false, bool = true, int_or_undef = 37, int = 27},
            ?assertEqual({ok, Expected}, typed_rec:from_json(Json))
        end},

        {"From json with atom labels", fun() ->
            Json = [
                {<<"bool_or_undef">>, false}, {<<"bool">>, true},
                {<<"int_or_undef">>, 37}, {<<"int">>, 27}
            ],
            Expected = #typed_rec{bool_or_undef = false, bool = true, int_or_undef = 37, int = 27},
            ?assertEqual({ok, Expected}, typed_rec:from_json(Json))
        end},

        {"From json, nulls converted to undefined", fun() ->
            Json = [
                {bool_or_undef, null}, {<<"bool">>, true},
                {<<"int_or_undef">>, null}, {int, 27}
            ],
            Expected = #typed_rec{bool_or_undef = undefined, bool = true, int_or_undef = undefined, int = 27},
            ?assertEqual({ok, Expected}, typed_rec:from_json(Json, [null_is_undefined]))
        end},

        {"From json, type mismatchs gives warning", fun() ->
            Json = [
                {bool_or_undef, <<"string">>}, {<<"bool">>, 27}, {int_or_undef, true}, {int, null}
            ],
            Expected = #typed_rec{bool_or_undef = <<"string">>, bool = 27, int_or_undef = true, int = null},
            Out = typed_rec:from_json(Json),
            ?assertMatch({ok, Expected, _WarnList}, Out),
            {ok, _, WarnList} = Out,
            ExpectedWarnings = [bool_or_undef, bool, int_or_undef, int],
            [?assert(lists:member(W, ExpectedWarnings)) || W <- WarnList]
        end},

        {"To json, undefined skipped", fun() ->
            Expected = [
                {bool, false},{int, 0}
            ],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec))
        end},

        {"To json, undefined to null", fun() ->
            Expected = [
                {bool_or_undef, null}, {bool, false},
                {int_or_undef, null}, {int, 0}
            ],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec, [{null_is_undefined}]))
        end},

        {"To json, appending key/values", fun() ->
            Expected = [
                {bool, false}, {int, 0}, {goober, <<"pants">>}, {<<"id">>, 7}
            ],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec, [{goober, <<"pants">>}, {<<"id">>, 7}]))
        end},

        {"To json, keys to remove", fun() ->
            Expected = [
                {int, 0}
            ],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec, [bool]))
        end},

        {"To json, all keys removed", fun() ->
            Expected = [{}],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec, [bool, int]))
        end},

        {"To json, functions to alter json", fun() ->
            Fun1 = fun(InJson) ->
                proplists:delete(bool, InJson)
            end,
            Fun2 = fun(InJson) ->
                [{propkey, false} | InJson]
            end,
            Expected = [
                {propkey, false},{int, 0}
            ],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec, [Fun1, Fun2]))
        end},

        {"To json, functions return empty list", fun() ->
            Fun1 = fun(_) ->
                []
            end,
            Expected = [{}],
            Rec = #typed_rec{},
            ?assertEqual(Expected, typed_rec:to_json(Rec, [Fun1]))
        end}

    ] end}.
