-module(rec2json_compile_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../test/rec2json_SUITE_data/hrl/basic.hrl").

compiled_module_test_() ->
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
