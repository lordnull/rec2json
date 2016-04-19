-module(suppress_property).
-compile({parse_transform, rec2json}).
-rec2json([{generate_type, false}]).

-record(suppress_property, {
	f1 = <<"hello">> :: binary(),
	f2 = 7 :: number()
}).
