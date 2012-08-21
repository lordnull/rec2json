-module(rec2json_triq).

-include_lib("triq/include/triq.hrl").

-compile(export_all).

basic() ->
    ?FORALL(BasicRec,
        {basic_rec, bool(), list(char()), list(int()), oneof([null, int()])},
        begin
            Rec = setelement(3, BasicRec, list_to_binary(element(3, BasicRec))),
            {ok, Rec} == basic_rec:from_json(jsx:to_term(jsx:to_json(Rec:to_json())))
        end
    ).

multi_rec() ->
    ?FORALL(Rec,
        oneof([
            {rec1, bool()},
            {rec2, int()}
        ]),
        begin
            DecodeMod = element(1, Rec),
            {ok, Rec} == DecodeMod:from_json(jsx:to_term(jsx:to_json(Rec:to_json())))
        end
    ).

int_typecheck() ->
    ?FORALL(Rec,
        {intchecked, oneof([bool(), int()])},
        case element(2, Rec) of
            Int when is_integer(Int) ->
                {ok, Rec} == intchecked:from_json(jsx:to_term(jsx:to_json(Rec:to_json())));
            _Bool ->
                {error, {intchecked, undefined}, [{counted, type_mismatch}]} == intchecked:from_json(jsx:to_term(jsx:to_json(Rec:to_json())))
        end).
