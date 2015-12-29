-type combo() :: integer() | boolean().

-record(included, {
    field
}).

-record(feature, {
    simple,
    default = <<"default">>,
    integer_type :: integer(),
    boolean_type :: boolean(),
    binary_type :: binary(),
    list_type :: [integer()],
    null_type :: 'null',
    record_type :: #included{},
    default_integer = 42 :: integer(),
    int_or_bool :: integer() | boolean(),
    over_zero :: pos_integer(),
    atoms :: 'init' | 'ready' | 'steady',
    int_min_max :: r2j_type:integer(-100, 100),
    included_records = [] :: [#included{}]
}).
