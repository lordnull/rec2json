-module(careful_tests).
-compile([{parse_transform, rec2json}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([to_json/1]).

-record(careful_tests, {
	one_field
}).

% the real test is if this compiles or not. By deafult, rec2json should
% not stomp on existing functions, even if this would prevent the module
% from working properly.

to_json(Rec) ->
	to_json([one_field], Rec).

is_careful_test() -> ?assert(true).

to_json_test() ->
	Rec = #careful_tests{one_field = <<"yo">>},
	?assertEqual([{}], to_json(Rec)).

-endif.
