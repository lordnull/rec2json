-module(test_person).

-compile([{parse_transform, rec2json}]).

-include("../test/test_rec.hrl").

-export([is_married/1]).

is_married(#test_person{spouse_id = undefined}) ->
	false;
is_married(_) ->
	true.