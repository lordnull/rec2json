-module(r2j_compile_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test/r2j_compile_tests.hrl").
-include("test/test_rec.hrl").

-export([point/1]).

-record(basic_rec, {
    boolean_thing,
    unicode_str,
    count,
    maybe_count
}).

compile_strings_test_() -> [
    {"simple record compile", fun() ->
        r2j_compile:scan_string("-record(cst1, {}).", []),
        code:load_file(cst1),
        ?assert(erlang:function_exported(cst1, to_json, 1))
    end},

    {"record with a type compile", fun() ->
        r2j_compile:scan_string("-record(cst2, {f :: pos_integer()}).", []),
        code:load_file(cst2),
        ?assert(erlang:function_exported(cst2, to_json, 1))
    end},

    {"two records", fun() ->
        r2j_compile:scan_string("-record(cst3_1, {f}).\n-record(cst3_2, {baz :: integer()}).", []),
        code:load_file(cst3_1),
        code:load_file(cst3_2),
        ?assert(erlang:function_exported(cst3_1, to_json, 1)),
        ?assert(erlang:function_exported(cst3_2, to_json, 1))
    end},

    {"a type and a record", fun() ->
        r2j_compile:scan_string("-type foobar() :: {pos_integer(), integer()}.\n-record(cst4, {foobar :: [foobar()]}).", []),
        code:load_file(cst4),
        ?assert(erlang:function_exported(cst4, to_json, 1))
    end},

    {"many different field repesentations", generator, fun() ->
        Types = [":: basic()", ":: atom", ":: 1", ":: exported:type()",
            ":: basic(1)", ":: exported:type(1)", ":: basic(basic(atom))",
            ":: exported:type(basic())", ":: exported:type(basic(1))",
            "= r2j:default() :: r2j:special()",
            "= r2j:special(3) :: r2j:special(3)",
            "= r2j:outer(r2j:special(3))",
            ":: r2j:special(3) | r2j:special(4)"],
        Seq = lists:seq(1, length(Types)),
        Keyed = lists:zip(Seq, Types),
        Keyed2 = [{integer_to_list(N), V} || {N, V} <- Keyed],
        types_gen(Keyed2)
    end},

    {"has an accessor function", fun() ->
        r2j_compile:scan_string("-record(cst5, {f}).", []),
        code:load_file(cst5),
        ?assert(erlang:function_exported(cst5, f, 1))
    end},

    {"no accessor when opted out", fun() ->
        r2j_compile:scan_string("-record(cst6, {f}).", [{generate_accessors, false}]),
        code:load_file(cst6),
        ?assertNot(erlang:function_exported(cst6, f, 1))
    end},

    {"record with various defaults defined", 
      lists:map(fun(DefaultStr) ->
        {DefaultStr, fun() ->
          Str = "-record(cst7, { f = " ++ DefaultStr ++ " }).",
          r2j_compile:scan_string(Str, [])
        end}
      end, ["1", "{1}", "\"string\"", "atom", "mod:func()", "mod:func(1)",
        "[1, 2, 3]", "{1, {}}", "[{}, 1, \"string\"]",
        "mod:func({})",
        "{{}}", "[a|b]"])
    }

    ].

types_gen([]) ->
    [];
types_gen([{Nth, Type} | Tail]) ->
    Generator = fun() ->
        types_gen(Tail)
    end,
    RecordStr = "-record(cst5_" ++ Nth ++ ", {f " ++ Type ++ "}).",
    Test = fun() ->
        r2j_compile:scan_string(RecordStr, []),
        Module = list_to_atom("cst5_" ++ Nth),
        code:load_file(Module),
        ?assert(erlang:function_exported(Module, to_json, 1))
    end,
    {generator, fun() -> [{Type, Test} | {generator, Generator}] end}.

parse_transform_test_() ->
    {setup, fun() ->
        ok
    end, fun(_) ->
        ok
    end, fun(_) -> [

        {"compile", fun() ->
            Got = compile:file("../test/test_rec"),
            ?assertEqual({ok, test_rec}, Got)
        end},

        {"The new module can be loaded", fun() ->
            {ok, _Module, Binary} = compile:file("../test/test_rec", [binary]),
            ?assertEqual({module, test_rec}, code:load_binary(test_rec, "../test/test_rec", Binary))
        end},

        {"has to_json/1", fun() ->
            ?assert(erlang:function_exported(test_rec, to_json, 1))
        end},

        {"compile a second with functions", fun() ->
            Got = compile:file("../test/test_person"),
            ?assertEqual({ok, test_person}, Got)
        end},

        {"Can load test_person", fun() ->
            {ok, _Module, Binary} = compile:file("../test/test_person", [binary]),
            ?assertEqual({module, test_person}, code:load_binary(test_person, "../test/test_person", Binary))
        end},

        {"has originally defined function", fun() ->
            ?assertNot(test_person:is_married(#test_person{}))
        end},

        {"has to_json/1", fun() ->
            ?assert(erlang:function_exported(test_person, to_json, 1))
        end},

        {"has accessor", fun() ->
            ?assert(erlang:function_exported(test_person, name, 1))
        end},

        {"has getters", fun() ->
            ?assert(erlang:function_exported(test_person, name, 2))
        end},

        {"compile and load with no accessors", fun() ->
            {ok, _Module, Binary} = compile:file("../test/test_person", [binary, {rec2json, [{generate_accessors, false}]}]),
            Got = code:load_binary(test_person, "../test/test_no_accessor", Binary),
            ?assertEqual({module, test_person}, Got)
        end},

        {"no accessor compile doesn't have accessor", fun() ->
            ?assertNot(erlang:function_exported(test_person, name, 1))
        end},

        {"no accessor still has to and from json", fun() ->
            ?assert(erlang:function_exported(test_person, to_json, 1)),
            ?assert(erlang:function_exported(test_person, from_json, 1))
        end},

        {"has getters", fun() ->
          ?assert(erlang:function_exported(test_person, name, 2))
        end},

        {"compile and load with no settors", fun() ->
            {ok, _Module, Binary} = compile:file("../test/test_person", [binary, {rec2json, [{generate_setters, false}]}]),
            Got = code:load_binary(test_person, "../test/test_no_setters", Binary),
            ?assertEqual({module, test_person}, Got)
        end},

        {"has no setters", fun() ->
            ?assertNot(erlang:function_exported(test_person, name, 2))
        end},

        {"no setters has accessor", fun() ->
            ?assert(erlang:function_exported(test_person, name, 1))
        end},

        {"no setters has to/from json", fun() ->
            ?assert(erlang:function_exported(test_person, to_json, 1)),
            ?assert(erlang:function_exported(test_person, from_json, 1))
        end},

        {"compile with no accessors or setters", fun() ->
            {ok, _Module, Binary} = compile:file("../test/test_person", [
                binary,
                {rec2json, [
                    {generate_setters, false},
                    {generate_accessors, false}
                ]}
            ]),
            Got = code:load_binary(test_person, "../test/test_no_getset", Binary),
            ?assertEqual({module, test_person}, Got)
        end},

        {"has no setters", fun() ->
            ?assertNot(erlang:function_exported(test_person, name, 1))
        end},

        {"has no getter", fun() ->
            ?assertNot(erlang:function_exported(test_person, name, 2))
        end},

        {"has to/from json", fun() ->
            ?assert(erlang:function_exported(test_person, to_json, 1)),
            ?assert(erlang:function_exported(test_person, from_json, 1))
        end}

    ] end}.

feature_test_() ->
    {setup, fun() ->
        r2j_compile:scan_file("../test/r2j_compile_tests.hrl", [])
    end, fun(_) ->
        ok
    end, fun(_) -> [

        {"to json as parameterized module", fun() ->
            Record = #included{},
            ?assertEqual([{}], Record:to_json())
        end},

        {"to json as regular module", fun() ->
            Record = #included{},
            ?assertEqual([{}], included:to_json(Record))
        end},

        {"to json as parametrized module, undefined is null", fun() ->
            Record = #included{},
            ?assertEqual([{field, null}], Record:to_json([{null_is_undefined}]))
        end},

        {"To json as regular module, undefined is null", fun() ->
            Record = #included{},
            ?assertEqual([{field, null}], included:to_json(Record, [{null_is_undefined}]))
        end},

        {"To json with adding prop mutator", fun() ->
            Record = #included{},
            Expected = [{newage, <<"hi">>}],
            ?assertEqual(Expected, included:to_json(Record, [{newage, <<"hi">>}]))
        end},

        {"To json with removing prop mutator", fun() ->
            Record = #included{field = <<"hi">>},
            Expected = [{}],
            ?assertEqual(Expected, included:to_json(Record, [field]))
        end},

        {"To json with arity 1 fun mutator", fun() ->
            Mutator = fun(J) ->
                ?assertEqual([], J),
                []
            end,
            Expected = [{}],
            Record = #included{},
            ?assertEqual(Expected, included:to_json(Record, [Mutator]))
        end},

        {"To json with arity 2 fun mutator", fun() ->
            Record = #included{},
            Mutator = fun(J, R) ->
                ?assertEqual([], J),
                ?assertEqual(Record, R),
                []
            end,
            ?assertEqual([{}], included:to_json(Record, [Mutator]))
        end},

        {"To json with all mutators", fun() ->
            Record = #included{field = 3},
            A1 = fun(J) ->
                ?assertEqual([{new1, <<"hi">>}], J),
                [{mut1, 1} | J]
            end,
            A2 = fun(J, R) ->
                ?assertEqual(Record, R),
                [{mut2, 2} | J]
            end,
            Mutators = [field, {new1, <<"hi">>}, A1, A2],
            Expected = [{mut2, 2}, {mut1, 1}, {new1, <<"hi">>}],
            ?assertEqual(Expected, included:to_json(Record, Mutators))
        end},

        {"To json, sub record encoded as empty obj", fun() ->
            IncRec = #included{},
            Record = #feature{default = undefined, default_integer = undefined, record_type = IncRec},
            ?assertEqual([{record_type, [{}]}], Record:to_json())
        end},

        {"To json, sub record fully encoded", fun() ->
            IncRec = #included{field = <<"field">>},
            Record = #feature{default = undefined, default_integer = undefined, record_type = IncRec},
            ?assertEqual([{record_type, [{field, <<"field">>}]}], Record:to_json())
        end},

        {"To json, atom becomes binary", fun() ->
          ?debugFmt("~p", [proplists:get_value(r2j_compile, code:all_loaded())]),
          Record = #included{field = 'an atom'},
          ?assertEqual([{field, <<"an atom">>}], Record:to_json())
        end},

        {"To json, true does not become binary", fun() ->
          ?debugFmt("~p", [proplists:get_value(r2j_compile, code:all_loaded())]),
          Record = #included{field = true},
          ?assertEqual([{field, true}], Record:to_json())
        end},

        {"To json, false does not become binary", fun() ->
          ?debugFmt("~p", [proplists:get_value(r2j_compile, code:all_loaded())]),
          Record = #included{field = false},
          ?assertEqual([{field, false}], Record:to_json())
        end},

        {"To json, empty lists stay lists", fun() ->
          Record = #included{field = []},
          ?assertEqual([{field, []}], Record:to_json())
        end},

        {"To json, empty object stay objects", fun() ->
          Record = #included{field = [{}]},
          ?assertEqual([{field, [{}]}], Record:to_json())
        end},

        {"from json with binary fields", fun() ->
            Expected = #included{field = <<"field">>},
            ?assertEqual({ok, Expected}, included:from_json([{<<"field">>, <<"field">>}]))
        end},

        {"from json with atom field", fun() ->
            Expected = #included{field = <<"field">>},
            ?assertEqual({ok, Expected}, included:from_json([{field, <<"field">>}]))
        end},

        {"from empty json object", fun() ->
            Expected = #included{},
            ?assertEqual({ok, Expected}, included:from_json([{}]))
        end},

        {"from json object with unspecified field", fun() ->
            Expected = #included{},
            Json = [{thang, 71}],
            ?assertEqual({ok, Expected}, included:from_json(Json))
        end},

        {"from json with seed record", fun() ->
            Seed = #included{field = 32},
            ?assertEqual({ok, Seed}, included:from_json(Seed, [{}]))
        end},

        {"from json with seed record as parameterized module", fun() ->
            Seed = #included{field = 32},
            ?assertEqual({ok, Seed}, Seed:from_json([{}]))
        end},

        {"from json with null as null", fun() ->
            Expected = #included{field = null},
            Json = [{field, null}],
            ?assertEqual({ok, Expected}, included:from_json(Json))
        end},

        {"from json with null as undefined", fun() ->
            Expected = #included{field = undefined},
            Json = [{field, null}],
            ?assertEqual({ok, Expected}, included:from_json(Json, [null_is_undefined]))
        end},

        {"from json with options and seed record", [
            {"parameterized module null is null", fun() ->
                Expected = #included{field = null},
                Json = [{field, null}],
                Seed = #included{field = 32},
                ?assertEqual({ok, Expected}, Seed:from_json(Json, []))
            end},

            {"parameterized module null is undefined", fun() ->
                Expected = #included{field = undefined},
                Json = [{field, null}],
                Seed = #included{field = 32},
                ?assertEqual({ok, Expected}, Seed:from_json(Json, [null_is_undefined]))
            end},

            {"pure module null is null", fun() ->
                Expected = #included{field = null},
                Json = [{field, null}],
                Seed = #included{field = 32},
                ?assertEqual({ok, Expected}, included:from_json(Json, [], Seed))
            end},

            {"pure module null is undefined", fun() ->
                Expected = #included{field = undefined},
                Json = [{field, null}],
                Seed = #included{field = 32},
                ?assertEqual({ok, Expected}, included:from_json(Json, [null_is_undefined], Seed))
            end}
        ]},

        {"from json with included record", fun() ->
            Expected = #feature{record_type = #included{}},
            Json = [{record_type, [{}]}],
            ?assertEqual({ok, Expected}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  integer", fun() ->
            Expected = #feature{integer_type = <<"hi">>},
            Json = [{integer_type, <<"hi">>}],
            ?assertEqual({ok, Expected, [integer_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  boolean", fun() ->
            Expected = #feature{boolean_type = 42},
            Json = [{boolean_type, 42}],
            ?assertEqual({ok, Expected, [boolean_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  binary", fun() ->
            Expected = #feature{binary_type = false},
            Json = [{binary_type, false}],
            ?assertEqual({ok, Expected, [binary_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch: list", fun() ->
            Expected = #feature{list_type = null},
            Json = [{list_type, null}],
            Got = feature:from_json(Json),
            ?assertEqual({ok, Expected, [list_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch: list 2", fun() ->
            Expected = #feature{list_type = [<<"hi">>]},
            Json = [{list_type, [<<"hi">>]}],
            ?assertEqual({ok, Expected, [[list_type, 1]]}, feature:from_json(Json))
        end},

        {"from json with type mismatch: null", fun() ->
            Expected = #feature{null_type = <<"hi">>},
            Json = [{null_type, <<"hi">>}],
            ?assertEqual({ok, Expected, [null_type]}, feature:from_json(Json))
        end},

        {"from json with type mismatch:  record_type", fun() ->
            Expected = #feature{record_type = 33},
            Json = [{record_type, 33}],
            ?assertEqual({ok, Expected, [record_type]}, feature:from_json(Json))
        end},

        {"Accessor functions", fun() ->
            Record = #feature{},
            Fields = record_info(fields, feature),
            NameAndN = lists:zip(Fields, lists:seq(2, length(Fields) + 1)),
            Test = fun({Accessor, Nth}) ->
                Expected = element(Nth, Record),
                ?assertEqual(Expected, Record:Accessor()),
                ?assertEqual(Expected, feature:Accessor(Record))
            end,
            lists:map(Test, NameAndN)
        end},

        {"setter functions", fun() ->
            Record = #feature{},
            Fields = record_info(fields, feature),
            NameAndN = lists:zip(Fields, lists:seq(2, length(Fields) + 1)),
            Test = fun({Setter, Nth}) ->
                R1 = Record:Setter(Nth),
                ?assertEqual(Nth, R1:Setter()),
                R2 = R1:Setter(goober),
                ?assertEqual(goober, R2:Setter())
            end,
            lists:map(Test, NameAndN)
        end},

        {"Field list function", fun() ->
            Fields = record_info(fields, feature),
            Got = feature:field_names(),
            ?assertEqual(Fields, Got)
        end},

        {"Types list", fun() ->
            Fields = record_info(fields, feature),
            Got = feature:field_types(),
            Types = [{any,[]}, {any,[]},
                {specific, [undefined, {r2j_type, integer, []}]},
                {specific, [undefined, {r2j_type, boolean, []}]},
                {specific, [undefined, {r2j_type, binary, []}]},
                {specific, [undefined, {list, {specific, [{r2j_type, integer, []}]}}]},
                {specific, [undefined, null]},
                {specific, [undefined, {record, included}]},
                {specific, [{r2j_type, integer, []}]},
                {specific, [undefined, {r2j_type, integer, []}, {r2j_type, boolean, []}]},
                {specific, [undefined, {r2j_type, pos_integer, []}]},
                {specific, [undefined, init, ready, steady]},
                {specific, [undefined, {r2j_type, integer, [-100, 100]}]}
            ],
            Zipped = lists:zip(Fields, Types),
            ?assertEqual(length(Zipped), length(Got)),
            GotZipped = lists:zip(Zipped, Got),
            lists:map(fun({Expected, Got}) ->
                ?assertEqual(Expected, Got)
            end, GotZipped)
        end}

    ] end}.

proper_test_() ->
    Exported = ?MODULE:module_info(exports),
    Exported1 = lists:filter(fun
        ({Atom, 0}) ->
            case atom_to_list(Atom) of
                "prop_" ++ _ -> true;
                _ -> false
            end;
        (_) ->
            false
    end, Exported),
    Exported2 = [F || {F, _} <- Exported1],
    proper_test_gen(Exported2).

proper_test_gen([]) ->
    {generator, fun() -> [] end};
proper_test_gen([ProperTest | Tail]) ->
    {generator, fun() -> [
        {atom_to_list(ProperTest), fun() ->
            ?assert(proper:quickcheck(erlang:apply(?MODULE, ProperTest, []), 100))
        end} |
        proper_test_gen(Tail) ]
    end}.

%% proper funcs.
prop_integer() ->
    r2j_compile:scan_string("-record(prop_integer, {f :: integer()}).", []),
    ?FORALL(Val, oneof([int(), real()]),
    begin
        Expected = {prop_integer, Val},
        Json = [{f, Val}],
        Got = prop_integer:from_json(Json),
        if
            is_integer(Val) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_pos_integer() ->
    r2j_compile:scan_string("-record(prop_pos_integer, {f :: pos_integer()}).", []),
    ?FORALL(Int, int(),
    begin
        Expected = {prop_pos_integer, Int},
        Json = [{f, Int}],
        Got = prop_pos_integer:from_json(Json),
        if
            Int > 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_int_or_bool() ->
    r2j_compile:scan_string("-record(prop_int_or_bool, {f :: integer() | boolean()}).", []),
    ?FORALL(IntOrBool, oneof([int(), bool(), binary(), real()]),
    begin
        Expected = {prop_int_or_bool, IntOrBool},
        Json = [{f, IntOrBool}],
        Got = prop_int_or_bool:from_json(Json),
        case IntOrBool of
            X when is_integer(X); is_boolean(X) ->
                {ok, Expected} == Got;
            _ ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_atoms() ->
    r2j_compile:scan_string("-record(prop_atoms, {f :: 'init' | 'ready' | 'steady'}).", []),
    ?FORALL(Atom, oneof([init, ready, steady, go, stop, hop]),
    begin
        Json = [{f, list_to_binary(atom_to_list(Atom))}],
        Got = prop_atoms:from_json(Json),
        case lists:member(Atom, [init, ready, steady]) of
            true ->
                Expected = {prop_atoms, Atom},
                {ok, Expected} == Got;
            false ->
                Expected = {prop_atoms, list_to_binary(atom_to_list(Atom))},
                {ok, Expected, [f]} == Got
        end
    end).

prop_non_neg_integer() ->
    r2j_compile:scan_string("-record(prop_non_neg_integer, {f :: non_neg_integer()}).", []),
    ?FORALL(Int, int(),
    begin
        Json = [{f, Int}],
        Got = prop_non_neg_integer:from_json(Json),
        Expected = {prop_non_neg_integer, Int},
        if
            Int < 0 ->
                {ok, Expected, [f]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_boolean() ->
    r2j_compile:scan_string("-record(prop_boolean, {f :: boolean()}).", []),
    ?FORALL(Bool, oneof([bool(), goober]),
    begin
        Expected = {prop_boolean, Bool},
        Json = [{f, Bool}],
        Got = prop_boolean:from_json(Json),
        if
            Bool == goober ->
                {ok, Expected,[f]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_neg_integer() ->
    r2j_compile:scan_string("-record(prop_neg_integer, {f :: neg_integer()}).", []),
    ?FORALL(Int, int(),
    begin
        Expected = {prop_neg_integer, Int},
        Json = [{f, Int}],
        Got = prop_neg_integer:from_json(Json),
        if
            Int < 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_number() ->
    r2j_compile:scan_string("-record(prop_number, {f :: number()}).", []),
    ?FORALL(Number, oneof([int(), real(), goober]),
    begin
        Expected = {prop_number, Number},
        Json = [{f, Number}],
        Got = prop_number:from_json(Json),
        if
            is_number(Number) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_binary() ->
    r2j_compile:scan_string("-record(prop_binary, {f :: binary()}).", []),
    ?FORALL(Binary, oneof([list(int()), binary()]),
    begin
        Expected = {prop_binary, Binary},
        Json = [{f, Binary}],
        Got = prop_binary:from_json(Json),
        if
            is_binary(Binary) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [f]} == Got
        end
    end).

prop_float() ->
    r2j_compile:scan_string("-record(prop_float, {f :: float()}).", []),
    ?FORALL(Float, oneof([int(), real()]),
    begin
        Expected = {prop_float, Float},
        Json = [{f, Float}],
        Got = prop_float:from_json(Json),
        if
            is_float(Float) ->
                {ok, Expected} == Got;
            true ->
                try Float * 1.0 of
                    _ ->
                        {ok, Expected} == Got
                catch
                    error:bararith ->
                        {ok, Expected, [f]} == Got
                end
        end
    end).

prop_list_one() ->
    r2j_compile:scan_string("-record(prop_list_one, {f :: [integer()]}).", []),
    ?FORALL(List, list(oneof([int(), real()])),
    begin
        Expected = {prop_list_one, List},
        Json = [{f, List}],
        Got = prop_list_one:from_json(Json),
        WarnsFun = fun
            (Item, _Ind, Acc) when is_integer(Item) ->
                Acc;
            (_Item, Ind, Acc) ->
                [[f, Ind] | Acc]
        end,
        case fold_ind(WarnsFun, [], List) of
            [] ->
                {ok, Expected} == Got;
            Warns ->
                Warns1 = lists:reverse(Warns),
                {ok, Expected, Warns1} == Got
        end
    end).

prop_record_one() ->
    r2j_compile:scan_string("-record(prop_record_one_outer, {f :: #prop_record_one_inner{}}).", []),
    r2j_compile:scan_string("-record(prop_record_one_inner, {f :: integer()}).", []),
    ?FORALL(SubRec, oneof([int(), [{}], [{f, oneof([int(), real()])}]]),
    begin
        {Json, Expected, Warns} = case SubRec of
            N when is_integer(N) ->
                {[{f, N}], {prop_record_one_outer, N}, [f]};
            [{f, N}] = Obj when is_integer(N) ->
                {[{f, Obj}], {prop_record_one_outer, {prop_record_one_inner, N}}, false};
            [{}] ->
                {[{f, [{}]}], {prop_record_one_outer, {prop_record_one_inner, undefined}}, false};
            [{f, N}] = Obj ->
                {[{f, Obj}], {prop_record_one_outer, {prop_record_one_inner, N}}, [[f, f]]}
        end,
        Got = prop_record_one_outer:from_json(Json),
        case Warns of
            false ->
                {ok, Expected} == Got;
            _ ->
                {ok, Expected, Warns} == Got
        end
    end).

prop_user_type() ->
    r2j_compile:scan_string("-record(prop_user_type, {f :: user_type()} ).", []),
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = [{<<"f">>, Val}],
        Expected = {prop_user_type, Val},
        Got = prop_user_type:from_json(Json),
        %?debugFmt("Json: ~p\nExpected: ~p; Got: ~p", [Json, Expected, Got]),
        {ok, Expected} == Got
    end).

prop_user_type_default() ->
    r2j_compile:scan_string("-record(prop_user_type_default, {f  = 3 :: user_type()} ).", []),
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = [{<<"f">>, Val}],
        Expected = {prop_user_type_default, Val},
        Got = prop_user_type_default:from_json(Json),
        %?debugFmt("Json: ~p\nExpected: ~p; Got: ~p", [Json, Expected, Got]),
        {ok, Expected} == Got
    end).

prop_user_type_list() ->
    r2j_compile:scan_string("-record(prop_user_type_list, {f :: [user_type()]} ).", []),
    ?FORALL(List, list({oneof([<<"a">>,<<"b">>,<<"c">>]), oneof([<<"bin">>, int(), real()])}),
    begin
        Json = [{<<"f">>, List}],
        Expected = {prop_user_type_list, List},
        Got = prop_user_type_list:from_json(Json),
        {ok, Expected} == Got
    end).

prop_r2j_integer_type() ->
    r2j_compile:scan_string("-record(prop_r2j_integer_type, {f :: r2j_type:integer()} ).", []),
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = [{<<"f">>, Val}],
        Rec = {prop_r2j_integer_type, Val},
        Expected = if
            is_integer(Val) ->
                {ok, Rec};
            true ->
                {ok, Rec, [f]}
        end,
        Got = prop_r2j_integer_type:from_json(Json),
        case Got of
            Expected ->
                try prop_r2j_integer_type:to_json(Rec) of
                    MaybeGood when is_integer(Val) ->
                       jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(MaybeGood,[]);
                    NotGood ->
                        ?debugFmt("expected a boom due to ~p but got ~p", [Val, NotGood]),
                        false
                catch
                    error:{badarg, {f, Val, {specific, [undefined, {r2j_type, integer, []}]}}} when not is_integer(Val) ->
                        true;
                    W:Y ->
                        ?debugFmt("Not the boom I expected: ~p:~p", [W,Y]),
                        false
                end;
            _ ->
                ?debugFmt("Expected: ~p; got ~p", [Expected, Got])
        end
    end).

prop_r2j_integer_min_max_type() ->
    r2j_compile:scan_string("-record(prop_r2j_integer_min_max_type, {f :: r2j_type:integer(-100, 100)} ).", []),
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = [{<<"f">>, Val}],
        Rec = {prop_r2j_integer_min_max_type, Val},
        Expected = if
            is_integer(Val), -100 =< Val, Val =< 100 ->
                {ok, Rec};
            true ->
                {ok, Rec, [f]}
        end,
        Got = prop_r2j_integer_min_max_type:from_json(Json),
        case Got of
            Expected ->
                try prop_r2j_integer_min_max_type:to_json(Rec) of
                    MaybeGood when is_integer(Val), -100 =< Val, Val =< 100 ->
                        jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(MaybeGood,[]);
                    NotGood ->
                        ?debugFmt("expected a boom due to ~p but got ~p", [Val, NotGood]),
                        false
                catch
                    error:{badarg, {f, Val, {specific, [undefined, {r2j_type, integer, [-100, 100]}]}}} when not is_integer(Val); Val < -100; 100 < Val ->
                        true;
                    W:Y ->
                        ?debugFmt("not the boom I expected: ~p:~p", [W,Y]),
                        false
                end;
            _ ->
                ?debugFmt("Expected: ~p; got: ~p", [Expected, Got])
        end
    end).

prop_r2j_min_max_listed() ->
    r2j_compile:scan_string("-record(prop_r2j_integer_min_max_listed, {f :: [r2j_type:integer(-100, 100)]} ).", []),
    ?FORALL(Val, list(oneof([<<"bin">>, int(), real()])),
    begin
        Json = [{<<"f">>, Val}],
        Rec = {prop_r2j_integer_min_max_listed, Val},
        FoldFun = fun
            (Int, _Index, Acc) when is_integer(Int), -100 =< Int, Int =< 100 ->
                Acc;
            (Bad, Index, Acc) ->
                [Index | Acc]
        end,
        Expected = case fold_ind(FoldFun, [], Val) of
            [] ->
                {ok, Rec};
            Warns ->
                RevWarns = lists:reverse(Warns),
                TaggedWarns = [[f, N] || N <- RevWarns],
                {ok, Rec, TaggedWarns}
        end,
        Got = prop_r2j_integer_min_max_listed:from_json(Json),
        IsGoodValue = lists:all(fun(N) ->
            is_integer(N) andalso -100 =< N andalso N =< 100
        end, Val),
        case Got of
            Expected ->
                try prop_r2j_integer_min_max_listed:to_json(Rec) of
                    Good when IsGoodValue ->
                        jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(Good,[]);
                    NotGood ->
                        ?debugFmt("expected a boom due to ~p but got ~p", [Val, NotGood]),
                        false
                catch
                    error:{badarg, {f, Val, {specific, [undefined, {list, {specific, [{r2j_type, integer, [-100, 100]}]}}]}}} when not IsGoodValue ->
                        true;
                    W:Y ->
                        ?debugFmt("not the boom I was expected: ~p:~p", [W,Y]),
                        false
                end;
            _ ->
                ?debugFmt("Expected: ~p; got ~p", [Expected, Got]),
                false
        end
    end).

prop_r2j_type_translation() ->
    r2j_compile:scan_string("-record(type_translation, {p :: r2j_compile_tests:point()} ).", []),
        ?FORALL({X, Y} = RecVal, {int(), int()},
        begin
        Json = [{<<"p">>, [{<<"x">>, X}, {<<"y">>, Y}]}],
        Rec = {type_translation, RecVal},
        Got = type_translation:from_json(Json),
        {ok, Rec} == Got andalso jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(type_translation:to_json(Rec),[])
    end).

fold_ind(Fun, Acc, List) ->
    fold_ind(Fun, Acc, 1, List).

fold_ind(_Fun, Acc, _Ind, []) ->
    Acc;
fold_ind(Fun, Acc, Ind, [Item | Tail]) ->
    Acc2 = Fun(Item, Ind, Acc),
    fold_ind(Fun, Acc2, Ind + 1, Tail).

point({X,Y}) ->
    {ok, [{x,X},{y,Y}]};

point(List) when is_list(List) ->
    X = proplists:get_value(x, List, proplists:get_value(<<"x">>, List)),
    Y = proplists:get_value(y, List, proplists:get_value(<<"y">>, List)),
    {ok, {X,Y}};

point(_) ->
    error.

