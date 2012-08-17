-module(rec2json_triq).

-include_lib("triq/include/triq.hrl").

-compile(export_all).

basic() ->
    ?FORALL(BasicRec,
        {basic_rec, bool(), unicode_binary(), list(int()), oneof([null, int()])},
        BasicRec == basic_rec:from_json(BasicRec:to_json())
    ).
