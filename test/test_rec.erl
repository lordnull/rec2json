-module(test_rec).

-compile([{parse_transform, rec2json}]).

-include("../test/test_rec.hrl").
