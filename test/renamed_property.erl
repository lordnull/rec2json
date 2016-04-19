-module(renamed_property).
-compile({parse_transform, rec2json}).
-rec2json({property_name, goober}).

-record(renamed_property, {
	f1 = 42 :: pos_integer(),
	f2 = hello :: hello | hi | yo | hey
}).
