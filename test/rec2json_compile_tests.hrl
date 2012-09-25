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
    combo_type :: combo(),
    atoms :: 'init' | 'ready' | 'steady'
}).
