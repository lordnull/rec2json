-module(default_property).
-compile([{parse_transfrom, rec2json}]).

-record(default_property, {
	f1 = 1 :: integer(),
	f2 = <<"hi">> :: binary()
}).
