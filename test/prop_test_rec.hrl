
-include("prop_test_rec_inner.hrl").

-type user_type() :: user_type.

-record(prop_test_rec, {
	prop_integer :: integer(),
	prop_pos_integer :: pos_integer(),
	prop_int_or_bool :: integer() | boolean(),
	prop_atoms :: init | ready | steady,
	prop_non_neg_integer :: non_neg_integer(),
	prop_boolean :: boolean(),
	prop_neg_integer :: neg_integer(),
	prop_number :: number(),
	prop_binary :: binary(),
	prop_float :: float(),
	prop_list_one :: [integer()],
	prop_record_one_inner :: #prop_test_rec_inner{},
	prop_record_list :: [#prop_test_rec_inner{}],
	prop_user_type :: user_type(),
	prop_user_type_default = 3 :: user_type(),
	prop_user_type_list :: [user_type()],
	prop_r2j_integer_type :: r2j_type:integer(),
	prop_r2j_integer_min_max_type :: r2j_type:integer(-100, 100),
	prop_r2j_integer_min_max_listed :: [r2j_type:integer(-100, 100)],
	type_translation :: r2j_compile_tests:point(),
	prop_null :: null,
	prop_any,
	prop_int_with_default = 1 :: integer()
}).
