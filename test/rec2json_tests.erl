-module(rec2json_tests).
-compile([{parse_transform, rec2json}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(sub_rec, {
	f
}).

-record(rec2json_tests, {
	typed_field = {1} :: rec2json_tests:int_tuple(),
	untyped_field = 3,
	missing_type = undefined :: nomod:nofun(),
	float_field = 1.0 :: float(),
	rec_field :: #sub_rec{},
	rec_field_list = [] :: [#sub_rec{}]
}).

-type int_tuple() :: any().

-export([int_tuple/1]).

int_tuple(Tuple) when is_tuple(Tuple) ->
	{ok, tuple_to_list(Tuple)};

int_tuple(List) when is_list(List) ->
	{ok, list_to_tuple(List)};

int_tuple(_) ->
	error.

feature_test_() -> [

		{"Has correct type info", fun() ->
			Types = ?MODULE:field_types(),
			?assertEqual({specific, [{?MODULE, int_tuple, []}]}, proplists:get_value(typed_field, Types)),
			?assertEqual({any, []}, proplists:get_value(untyped_field, Types)),
			?assertEqual({specific, [{nomod, nofun, []}]}, proplists:get_value(missing_type, Types))
		end},

		{"does to_json conversion correctly", fun() ->
			Rec = #rec2json_tests{typed_field = {2, 3, 4}, untyped_field = <<"hi">>},
			Expected = [{typed_field, [2, 3, 4]}, {untyped_field, <<"hi">>}, {float_field, 1.0}, {rec_field_list, []}],
			Got = ?MODULE:to_json(Rec),
			?assertEqual(Expected, Got)
		end},

		{"does from_json conversion correctly", fun() ->
			Json = [{<<"typed_field">>, [4, 3, 2]}, {<<"untyped_field">>, <<"bye">>}],
			Expected = #rec2json_tests{typed_field = {4, 3, 2}, untyped_field = <<"bye">>},
			Got = ?MODULE:from_json(Json),
			?assertEqual({ok, Expected}, Got)
		end},

		{"skips over non-rec2json records", fun() ->
			Json = [{rec_field_list, [
				[{f, 1}],
				[{f, 2}],
				[{f, 3}]
			]}],
			Expected = #rec2json_tests{rec_field_list = [[{f, 1}], [{f, 2}], [{f, 3}]]},
			% we still get warnings because the type spec of the list does not
			% have a valid type that matches the json (like any()).
			ExpectedWarnings = [
				[rec_field_list, 1],
				[rec_field_list, 2],
				[rec_field_list, 3]
			],
			Got = ?MODULE:from_json(Json),
			?assertEqual({ok, Expected, ExpectedWarnings}, Got)
		end},

		{"if a field is skipped, don't bother to verify it's type", fun() ->
			Record = #rec2json_tests{rec_field_list = {not_valid}},
			Expected = [{typed_field, [1]}, {untyped_field, 3}, {float_field, 1.0}],
			Got = ?MODULE:to_json([rec_field_list], Record),
			?assertEqual(Expected, Got)
		end},

		{"can to_json with types loads needed module", fun() ->
			code:delete(r2j_type),
			code:purge(r2j_type),
			false = code:is_loaded(r2j_type),
    	Rec = #?MODULE{},
			Got = Rec:to_json(),
			?assertEqual(1.0, proplists:get_value(float_field, Got))
		end}

	].

-endif.
