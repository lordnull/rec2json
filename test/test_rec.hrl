-record(test_rec, {
	id,
	name :: binary(),
	counter = 1
}).

-record(test_person, {
	id :: pos_integer(),
	name = <<"gustav">> :: binary(),
	age = 0 :: non_neg_integer(),
	spouse_id :: pos_integer()
}).