-module(default_value_tests).
-compile([{parse_transform, rec2json}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-record(sub_rec, {}).
-record(record_name, {field_name = <<"record_name_field">>}).

-record(default_value_tests, {
    integer_no_type = 3,
    integer_with_type = 7 :: integer(),
    binary_no_type = <<"yo">>,
    binary_with_type = <<"heya">> :: binary(),
    integer_list_no_type = [8,3],
    integer_list_with_type = [27, 89, 32] :: [integer()],
    tuple = {undefined, 3},
    list_of_tuples = [{1,2,3}, {4,5,6}, {7,8,0}],
    list_of_list_of_integers = [[1,2,3], [4,5,6], [7,8,9]],
    record_name = #record_name{},
    record_name_field_is_1 = #record_name{field_name = 1},
    record_with_sub_rec = #record_name{field_name = #sub_rec{}},
    record_part_of_tuple = {1, #record_name{}},
    record_part_of_list = [#record_name{}, 1, "Der string"],
    local_func_call = make_default(),
    local_func_call_with_arg = make_default(27),
    remote_func_call = ?MODULE:make_default(),
  remote_func_call_with_arg = ?MODULE:make_default(87),
    cons = [a|b]
}).

-export([make_default/0, make_default/1]).

make_default() ->
    make_default(ok).

make_default(Val) ->
    {default, Val}.

defaults_test_() ->
    {ok, Rec} = default_value_tests:from_json(#{}),

    Expectations = [
        {integer_no_type, 3},
        {integer_with_type, 7},
        {binary_no_type, <<"yo">>},
        {binary_with_type, <<"heya">>},
        {integer_list_no_type, [8,3]},
        {integer_list_with_type, [27, 89, 32]},
        {tuple, {undefined, 3}},
        {list_of_tuples, [{1,2,3}, {4,5,6}, {7,8,0}]},
        {list_of_list_of_integers, [[1,2,3], [4,5,6], [7,8,9]]},
        {record_name, #record_name{}},
        {record_name_field_is_1, #record_name{field_name = 1}},
        {record_with_sub_rec, #record_name{field_name = #sub_rec{}}},
        {record_part_of_tuple, {1, #record_name{}}},
        {record_part_of_list, [#record_name{}, 1, "Der string"]},
        {local_func_call, make_default()},
        {local_func_call_with_arg, make_default(27)},
        {remote_func_call, ?MODULE:make_default()},
        {remote_func_call_with_arg, ?MODULE:make_default(87)},
        {cons, [a|b]}
    ],

    lists:map(fun({Field, Value}) ->
        Name = iolist_to_binary(io_lib:format("ensure ~p is ~p", [Field, Value])),
        {Name, fun() ->
            ?assertEqual(Value, erlang:apply(Rec, Field, []))
        end}
    end, Expectations).

-endif.

