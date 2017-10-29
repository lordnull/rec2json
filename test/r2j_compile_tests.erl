-module(r2j_compile_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test/r2j_compile_tests.hrl").
-include("test/test_rec.hrl").
-include("prop_test_rec.hrl").

-type point() :: {float(), float()}.
-export_type([point/0]).
-export([point/1]).

-define(TEST_DIRECTORY, "test/").

types_gen([]) ->
    [];
types_gen([{Nth, Type} | Tail]) ->
    Generator = fun() ->
        types_gen(Tail)
    end,
    RecordStr = "-record(cst5_" ++ Nth ++ ", {f " ++ Type ++ "}).",
    Test = fun() ->
        r2j_compile:scan_string(RecordStr, [{output_dir, ?TEST_DIRECTORY}]),
        Module = list_to_atom("cst5_" ++ Nth),
        code:load_file(Module),
        ?assert(erlang:function_exported(Module, to_json, 1))
    end,
    {generator, fun() -> [{Type, Test} | {generator, Generator}] end}.

parse_transform_test_() ->
    [

        % while the module is likely already compiled once, we don't want to
        % have to potentially keep doing a clean just to ensure changes to the
        % parse transform don't break stuff, thus the recompiles here.
        {"compile", fun() ->
            Got = compile:file(?TEST_DIRECTORY ++ "test_rec", [{outdir, ?TEST_DIRECTORY}]),
            ?assertEqual({ok, test_rec}, Got)
        end},

        {"The new module can be loaded", fun() ->
            {ok, _Module, Binary} = compile:file(?TEST_DIRECTORY ++ "test_rec", [binary]),
            ?assertEqual({module, test_rec}, code:load_binary(test_rec, ?TEST_DIRECTORY ++ "test_rec", Binary))
        end},

        {"has to_json/1", fun() ->
            ?assert(erlang:function_exported(test_rec, to_json, 1))
        end},

        {"compile a second with functions", fun() ->
            Got = compile:file(?TEST_DIRECTORY ++ "test_person", [{outdir, ?TEST_DIRECTORY}]),
            ?assertEqual({ok, test_person}, Got)
        end},

        {"Can load test_person", fun() ->
            {ok, _Module, Binary} = compile:file(?TEST_DIRECTORY ++ "test_person", [binary]),
            ?assertEqual({module, test_person}, code:load_binary(test_person, ?TEST_DIRECTORY ++ "test_person", Binary))
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
            {ok, _Module, Binary} = compile:file(?TEST_DIRECTORY ++ "test_person", [binary, {rec2json, [{generate_accessors, false}]}]),
            Got = code:load_binary(test_person, ?TEST_DIRECTORY ++ "test_no_accessor", Binary),
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
            {ok, _Module, Binary} = compile:file(?TEST_DIRECTORY ++ "test_person", [binary, {rec2json, [{generate_setters, false}]}]),
            Got = code:load_binary(test_person, ?TEST_DIRECTORY ++ "test_no_setters", Binary),
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
            {ok, _Module, Binary} = compile:file(?TEST_DIRECTORY ++ "test_person", [
                binary,
                {rec2json, [
                    {generate_setters, false},
                    {generate_accessors, false}
                ]}
            ]),
            Got = code:load_binary(test_person, ?TEST_DIRECTORY ++ "test_no_getset", Binary),
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
        end},

        {"has default conversion function", fun() ->
            Exported = default_property:module_info(exports),
            ?assert(lists:member({default_property, 1}, Exported))
        end},

        {"default property conversion works as expected", fun() ->
            InitialJson = #{},
            {ok, Record} = default_property:default_property(InitialJson),
            1 = default_property:f1(Record),
            <<"hi">> = default_property:f2(Record),
            EmptyRec = default_property:f1(undefined, default_property:f2(undefined, Record)),
            {ok, #{}} = default_property:default_property(EmptyRec)
        end},

        {"property name can be altered", fun() ->
            Exported = renamed_property:module_info(exports),
            ?assert(lists:member({goober, 1}, Exported))
        end},

        {"renamed property works like default", fun() ->
            InitialJson = #{},
            {ok, Record} = renamed_property:goober(InitialJson),
            42 = renamed_property:f1(Record),
            hello = renamed_property:f2(Record),
            EmptyRec = renamed_property:f1(undefined, renamed_property:f2(undefined, Record)),
            {ok, #{}} = renamed_property:goober(EmptyRec)

        end},

        {"property creation suppressed", fun() ->
            Exported = suppress_property:module_info(exports),
            ?assertNot(lists:member({suppress_property, 1}, Exported))
        end}

    ].

feature_test_() -> [
        {"to json", fun() ->
            Record = #prop_test_rec{},
            ?assertEqual(#{prop_int_with_default => 1, prop_user_type_default => 3}, prop_test_rec:to_json(Record))
        end},

        {"to json, undefined is null", fun() ->
            Record = #prop_test_rec{},
            [prop_test_rec | OtherValues] = tuple_to_list(Record),
            NulledValues = lists:map(fun
                (undefined) -> null;
                (V) -> V
            end, OtherValues),
            Json = prop_test_rec:to_json([{null_is_undefined}], Record),
            JsonAsList = maps:to_list(Json),
            Winnowed = lists:foldl(fun({_, V}, Acc) ->
                Acc -- [V]
            end, NulledValues, JsonAsList),
            ?assertEqual([], Winnowed)
        end},

        {"To json with adding prop mutator", fun() ->
            Record = #prop_test_rec{},
            ?assertMatch(#{newage := <<"hi">>}, prop_test_rec:to_json(Record, [{newage, <<"hi">>}]))
        end},

        {"To json with adding prop mutator as map", fun() ->
            Record = #prop_test_rec{},
            ?assertMatch(#{foo := <<"bar">>, baz := 3}, prop_test_rec:to_json(Record, [#{foo => <<"bar">>, baz => 3}]))
        end},

        {"To json with removing prop mutator", fun() ->
            Record = #prop_test_rec{prop_integer = 75},
            Json = prop_test_rec:to_json(Record, [prop_integer]),
            ?assertEqual(error, maps:find(prop_integer, Json))
        end},

        {"To json with arity 1 fun mutator", fun() ->
            Mutator1 = fun(_) ->
               #{}
            end,
            Mutator2 = fun(J) ->
                J#{fart_jokes => true}
            end,
            Expected = #{fart_jokes => true},
            Record = #prop_test_rec{},
            ?assertEqual(Expected, prop_test_rec:to_json(Record, [Mutator1, Mutator2]))
        end},

        {"To json with arity 2 fun mutator", fun() ->
            Record = #prop_test_rec{},
            Mutator = fun(_, R) ->
                ?assertEqual(Record, R),
                #{flerb => 5}
            end,
            ?assertEqual(#{flerb => 5}, prop_test_rec:to_json(Record, [Mutator]))
        end},

        {"To json with all mutators", fun() ->
            Record = #prop_test_rec{prop_integer = 3},
            A1 = fun(J) ->
                ?assertMatch(#{new1 := <<"hi">>}, J),
                J#{mut1 => 1}
            end,
            A2 = fun(J, R) ->
                ?assertEqual(Record, R),
                J#{mut2 => 2}
            end,
            Mutators = [field, prop_int_with_default, prop_user_type_default, prop_integer, {new1, <<"hi">>}, A1, A2, #{new2 => 94, new3 => <<"yo">>}],
            Expected = #{mut2 => 2, mut1 => 1, new1 => <<"hi">>, new2 => 94, new3 => <<"yo">>},
            ?assertEqual(Expected, prop_test_rec:to_json(Record, Mutators))
        end},

        {"To json, sub record encoded as empty obj", fun() ->
            IncRec = #prop_test_rec_inner{},
            Record = #prop_test_rec{prop_record_one_inner = IncRec},
            ?assertMatch(#{prop_record_one_inner := #{}}, prop_test_rec:to_json(Record, []))
        end},

        {"To json, sub record fully encoded", fun() ->
            IncRec = #prop_test_rec_inner{f = 7},
            Record = #prop_test_rec{prop_record_one_inner = IncRec},
            ?assertMatch(#{prop_record_one_inner := #{f := 7}}, prop_test_rec:to_json(Record))
        end},

        {"To json, atom becomes binary", fun() ->
          ?debugFmt("~p", [proplists:get_value(r2j_compile, code:all_loaded())]),
          Record = #prop_test_rec{prop_atoms = init},
          ?assertMatch(#{prop_atoms := <<"init">>}, prop_test_rec:to_json(Record))
        end},

        {"To json, true does not become binary", fun() ->
          ?debugFmt("~p", [proplists:get_value(r2j_compile, code:all_loaded())]),
          Record = #prop_test_rec{prop_boolean = true},
          ?assertMatch(#{prop_boolean := true}, prop_test_rec:to_json(Record))
        end},

        {"To json, false does not become binary", fun() ->
          ?debugFmt("~p", [proplists:get_value(r2j_compile, code:all_loaded())]),
          Record = #prop_test_rec{prop_boolean = false},
          ?assertMatch(#{prop_boolean := false}, prop_test_rec:to_json(Record))
        end},

        {"To json, empty lists stay lists", fun() ->
          Record = #prop_test_rec{prop_list_one = []},
          ?assertMatch(#{prop_list_one := []}, prop_test_rec:to_json(Record))
        end},

        {"from json with binary fields", fun() ->
            Expected = #prop_test_rec{prop_binary = <<"field">>},
            {ok, Got} = prop_test_rec:from_json(#{<<"prop_binary">> => <<"field">>}),
            ?assertEqual(<<"field">>, Got#prop_test_rec.prop_binary),
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(#{<<"prop_binary">> => <<"field">>}))
        end},

        {"from json with atom field", fun() ->
            Expected = #prop_test_rec{prop_binary = <<"field">>},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(#{prop_binary => <<"field">>}))
        end},

        {"from empty json object", fun() ->
            Expected = #prop_test_rec{},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(#{}))
        end},

        {"from json object with unspecified field", fun() ->
            Expected = #prop_test_rec{},
            Json = #{thang => 71},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json))
        end},

        {"from json with seed record", fun() ->
            Seed = #prop_test_rec{prop_integer = 32},
            ?assertEqual({ok, Seed}, prop_test_rec:from_json(Seed, #{}))
        end},

        {"from json with null as null", fun() ->
            Expected = #prop_test_rec{prop_integer = null},
            Json = #{prop_integer => null},
            ?assertEqual({ok, Expected, [prop_integer]}, prop_test_rec:from_json(Json))
        end},

        {"from json with null as undefined", fun() ->
            Expected = #prop_test_rec{prop_integer = undefined},
            Json = #{prop_integer => null},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json, [null_is_undefined]))
        end},

        {"from json with options and seed record", [
            {"pure module null is null", fun() ->
                Expected = #prop_test_rec{prop_null = null},
                Json = #{prop_null => null},
                Seed = #prop_test_rec{prop_null = 32},
                ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json, [], Seed))
            end},

            {"pure module null is undefined", fun() ->
                Expected = #prop_test_rec{prop_integer = undefined},
                Json = #{prop_integer =>  null},
                Seed = #prop_test_rec{prop_integer = 32},
                ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json, [null_is_undefined], Seed))
            end}
        ]},

        {"from json with included record", fun() ->
            Expected = #prop_test_rec{prop_record_one_inner = #prop_test_rec_inner{}},
            Json = #{prop_record_one_inner => #{}},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json))
        end},

        {"from json with a list of included records", fun() ->
            Expected = #prop_test_rec{prop_record_list = [
                #prop_test_rec_inner{f = 1},
                #prop_test_rec_inner{f = 2}
            ]},
            Json = #{prop_record_list => [
                #{f => 1},
                #{f => 2}
            ]},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json))
        end},

        {"from json with type mismatch:  integer", fun() ->
            Expected = #prop_test_rec{prop_integer = <<"hi">>},
            Json = #{prop_integer => <<"hi">>},
            ?assertEqual({ok, Expected, [prop_integer]}, prop_test_rec:from_json(Json))
        end},

        {"from json with type mismatch:  boolean", fun() ->
            Expected = #prop_test_rec{prop_boolean = 42},
            Json = #{prop_boolean => 42},
            ?assertEqual({ok, Expected, [prop_boolean]}, prop_test_rec:from_json(Json))
        end},

        {"from json with type mismatch:  binary", fun() ->
            Expected = #prop_test_rec{prop_binary = false},
            Json = #{prop_binary => false},
            ?assertEqual({ok, Expected, [prop_binary]}, prop_test_rec:from_json(Json))
        end},

        {"from json with type mismatch: list", fun() ->
            Expected = #prop_test_rec{prop_list_one = <<"vida loca">>},
            Json = #{prop_list_one => <<"vida loca">>},
            Got = prop_test_rec:from_json(Json),
            ?assertEqual({ok, Expected, [prop_list_one]}, Got)
        end},

        {"from json with type mismatch: list 2", fun() ->
            Expected = #prop_test_rec{prop_list_one= [<<"hi">>]},
            Json = #{prop_list_one => [<<"hi">>]},
            ?assertEqual({ok, Expected, [[prop_list_one, 1]]}, prop_test_rec:from_json(Json))
        end},

        {"from json with type mismatch: null", fun() ->
            Expected = #prop_test_rec{prop_null = <<"hi">>},
            Json = #{prop_null => <<"hi">>},
            ?assertEqual({ok, Expected, [prop_null]}, prop_test_rec:from_json(Json))
        end},

        {"from json with type mismatch:  record_type", fun() ->
            Expected = #prop_test_rec{prop_record_one_inner = 33},
            Json = #{prop_record_one_inner => 33},
            ?assertEqual({ok, Expected, [prop_record_one_inner]}, prop_test_rec:from_json(Json))
        end},

        {"from json any field causes no warnings", fun() ->
            Expected = #prop_test_rec{prop_any = #{f => 4}},
            Json = #{prop_any => #{ f => 4}},
            ?assertEqual({ok, Expected}, prop_test_rec:from_json(Json))
        end},

        {"Accessor functions", fun() ->
            UnsetRecord = #prop_test_rec_inner{},
            Fields = record_info(fields, prop_test_rec_inner),
            NameAndN = lists:zip(Fields, lists:seq(2, length(Fields) + 1)),
            Record = lists:foldl(fun({_, N}, Acc) ->
                Rando = rand:uniform(),
                setelement(N, Acc, Rando)
            end, UnsetRecord, NameAndN),
            Test = fun({Accessor, Nth}) ->
                Expected = element(Nth, Record),
                ?assertEqual(Expected, prop_test_rec_inner:Accessor(Record))
            end,
            lists:map(Test, NameAndN)
        end},

        {"setter functions", fun() ->
            Record = #prop_test_rec{},
            Fields = record_info(fields, prop_test_rec),
            NameAndN = lists:zip(Fields, lists:seq(2, length(Fields) + 1)),
            Test = fun({Setter, Nth}) ->
                R1 = prop_test_rec:Setter(Nth, Record),
                ?assertEqual(Nth, prop_test_rec:Setter(R1)),
                R2 = prop_test_rec:Setter(goober, R1),
                ?assertEqual(goober, prop_test_rec:Setter(R2))
            end,
            lists:map(Test, NameAndN)
        end},

        {"Field list function", fun() ->
            Fields = record_info(fields, prop_test_rec),
            Got = prop_test_rec:field_names(),
            ?assertEqual(Fields, Got)
        end},

        {"Types list", fun() ->
            Fields = record_info(fields, prop_test_rec),
            Got = prop_test_rec:field_types(),
            Types = [
                {specific, [undefined, {r2j_type, integer, []}]},
                {specific, [undefined, {r2j_type, pos_integer, []}]},
                {specific, [undefined, {r2j_type, integer, []}, {r2j_type, boolean, []}]},
                {specific, [undefined, init, ready, steady]},
                {specific, [undefined, {r2j_type, non_neg_integer, []}]},
                {specific, [undefined, {r2j_type, boolean, []}]},
                {specific, [undefined, {r2j_type, neg_integer, []}]},
                {specific, [undefined, {r2j_type, number, []}]},
                {specific, [undefined, {r2j_type, binary, []}]},
                {specific, [undefined, {r2j_type, float, []}]},
                {specific, [undefined, {list, {specific, [{r2j_type, integer, []}]}}]},
                {specific, [undefined, {record, prop_test_rec_inner}]},
                {specific, [undefined, {list, {specific, [{record, prop_test_rec_inner}]}}]},
                {any, [undefined]},
                {any, []},
                {specific, [undefined, {list, any}]},
                {specific, [undefined, {r2j_type, integer, []}]},
                {specific, [undefined, {r2j_type, integer, [-100, 100]}]},
                {specific, [undefined, {list, {specific, [{r2j_type, integer, [-100, 100]}]}}]},
                {specific, [undefined, {r2j_compile_tests, point, []}]},
                {specific, [undefined, null]},
                {any, [undefined]},
                {specific, [{r2j_type, integer,[]}]}
            ],
            Zipped = lists:zip(Fields, Types),
            ?assertEqual(length(Zipped), length(Got)),
            GotZipped = lists:zip(Zipped, Got),
            lists:map(fun({Expected, G}) ->
                ?assertEqual(Expected, G)
            end, GotZipped)
        end}

    ].

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
    ?FORALL(Val, oneof([int(), real()]),
    begin
        Expected = #prop_test_rec{prop_integer = Val},
        Json = #{prop_integer => Val},
        Got = prop_test_rec:from_json(Json),
        if
            is_integer(Val) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [prop_integer]} == Got
        end
    end).

prop_pos_integer() ->
    ?FORALL(Int, int(),
    begin
        Expected = #prop_test_rec{prop_pos_integer = Int},
        Json = #{prop_pos_integer => Int},
        Got = prop_test_rec:from_json(Json),
        if
            Int > 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [prop_pos_integer]} == Got
        end
    end).

prop_int_or_bool() ->
    ?FORALL(IntOrBool, oneof([int(), bool(), binary(), real()]),
    begin
        Expected = #prop_test_rec{prop_int_or_bool = IntOrBool},
        Json = #{prop_int_or_bool => IntOrBool},
        Got = prop_test_rec:from_json(Json),
        case IntOrBool of
            X when is_integer(X); is_boolean(X) ->
                {ok, Expected} == Got;
            _ ->
                {ok, Expected, [prop_int_or_bool]} == Got
        end
    end).

prop_atoms() ->
    ?FORALL(Atom, oneof([init, ready, steady, go, stop, hop]),
    begin
        Json = #{prop_atoms => list_to_binary(atom_to_list(Atom))},
        Got = prop_test_rec:from_json(Json),
        case lists:member(Atom, [init, ready, steady]) of
            true ->
                Expected = #prop_test_rec{prop_atoms = Atom},
                {ok, Expected} == Got;
            false ->
                Expected = #prop_test_rec{prop_atoms = list_to_binary(atom_to_list(Atom))},
                {ok, Expected, [prop_atoms]} == Got
        end
    end).

prop_non_neg_integer() ->
    ?FORALL(Int, int(),
    begin
        Json = #{prop_non_neg_integer => Int},
        Got = prop_test_rec:from_json(Json),
        Expected = #prop_test_rec{prop_non_neg_integer = Int},
        if
            Int < 0 ->
                {ok, Expected, [prop_non_neg_integer]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_boolean() ->
    ?FORALL(Bool, oneof([bool(), goober]),
    begin
        Expected = #prop_test_rec{prop_boolean = Bool},
        Json = #{prop_boolean => Bool},
        Got = prop_test_rec:from_json(Json),
        if
            Bool == goober ->
                {ok, Expected,[prop_boolean]} == Got;
            true ->
                {ok, Expected} == Got
        end
    end).

prop_neg_integer() ->
    ?FORALL(Int, int(),
    begin
        Expected = #prop_test_rec{prop_neg_integer = Int},
        Json = #{prop_neg_integer => Int},
        Got = prop_test_rec:from_json(Json),
        if
            Int < 0 ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [prop_neg_integer]} == Got
        end
    end).

prop_number() ->
    ?FORALL(Number, oneof([int(), real(), goober]),
    begin
        Expected = #prop_test_rec{prop_number = Number},
        Json = #{prop_number => Number},
        Got = prop_test_rec:from_json(Json),
        if
            is_number(Number) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [prop_number]} == Got
        end
    end).

prop_binary() ->
    ?FORALL(Binary, oneof([list(int()), binary()]),
    begin
        Expected = #prop_test_rec{prop_binary = Binary},
        Json = #{prop_binary => Binary},
        Got = prop_test_rec:from_json(Json),
        if
            is_binary(Binary) ->
                {ok, Expected} == Got;
            true ->
                {ok, Expected, [prop_binary]} == Got
        end
    end).

prop_float() ->
    ?FORALL(Float, oneof([int(), real()]),
    begin
        Expected = #prop_test_rec{prop_float = Float},
        Json = #{prop_float => Float},
        Got = prop_test_rec:from_json(Json),
        if
            is_float(Float) ->
                {ok, Expected} == Got;
            true ->
                try Float * 1.0 of
                    _ ->
                        {ok, Expected} == Got
                catch
                    error:bararith ->
                        {ok, Expected, [prop_float]} == Got
                end
        end
    end).

prop_list_one() ->
    ?FORALL(List, list(oneof([int(), real()])),
    begin
        Expected = #prop_test_rec{prop_list_one = List},
        Json = #{prop_list_one => List},
        Got = prop_test_rec:from_json(Json),
        WarnsFun = fun
            (Item, _Ind, Acc) when is_integer(Item) ->
                Acc;
            (_Item, Ind, Acc) ->
                [[prop_list_one, Ind] | Acc]
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
    ?FORALL(SubRec, oneof([int(), #{}, #{prop_record_one_inner => oneof([int(), real()])}]),
    begin
        {Json, Expected, Warns} = case SubRec of
            N when is_integer(N) ->
                {#{prop_record_one_inner => N}, #prop_test_rec{prop_record_one_inner = N}, [prop_record_one_inner]};
            #{f := N} = Obj when is_integer(N) ->
                {#{prop_record_one_inner => Obj}, #prop_test_rec{prop_record_one_inner = #prop_test_rec_inner{f = N}}, false};
            #{} ->
                {#{prop_record_one_inner => #{}}, #prop_test_rec{prop_record_one_inner = #prop_test_rec_inner{f = undefined}}, false};
            #{f := N} = Obj ->
                {#{prop_record_one_inner => Obj}, #prop_test_rec{prop_record_one_inner = #prop_test_rec_inner{f = N}}, [[prop_record_one_inner, f]]}
        end,
        Got = prop_test_rec:from_json(Json),
        case Warns of
            false ->
                {ok, Expected} == Got;
            _ ->
                {ok, Expected, Warns} == Got
        end
    end).

prop_user_type() ->
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = #{<<"prop_user_type">> => Val},
        Expected = #prop_test_rec{prop_user_type = Val},
        Got = prop_test_rec:from_json(Json),
        {ok, Expected} == Got
    end).

prop_user_type_default() ->
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = #{<<"prop_user_type_default">> => Val},
        Expected = #prop_test_rec{prop_user_type_default = Val},
        Got = prop_test_rec:from_json(Json),
        {ok, Expected} == Got
    end).

prop_user_type_list() ->
    ?FORALL(List, list({oneof([<<"a">>,<<"b">>,<<"c">>]), oneof([<<"bin">>, int(), real()])}),
    begin
        Json = #{<<"prop_user_type_list">> => List},
        Expected = #prop_test_rec{prop_user_type_list = List},
        Got = prop_test_rec:from_json(Json),
        {ok, Expected} == Got
    end).

prop_r2j_integer_type() ->
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = #{<<"prop_r2j_integer_type">> => Val},
        Rec = #prop_test_rec{prop_r2j_integer_type = Val},
        Expected = if
            is_integer(Val) ->
                {ok, Rec};
            true ->
                {ok, Rec, [prop_r2j_integer_type]}
        end,
        Trans = fun(J) ->
            maps:filter(fun(Key, _) -> Key =:= prop_r2j_integer_type end, J)
        end,
        Got = prop_test_rec:from_json(Json),
        case Got of
            Expected ->
                try prop_test_rec:to_json(Rec, [Trans]) of
                    MaybeGood when is_integer(Val) ->
                       jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(MaybeGood,[]);
                    NotGood ->
                        ?debugFmt("expected a boom due to ~p but got ~p", [Val, NotGood]),
                        false
                catch
                    error:{badarg, {prop_r2j_integer_type, Val, {specific, [undefined, {r2j_type, integer, []}]}}} when not is_integer(Val) ->
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
    ?FORALL(Val, oneof([<<"bin">>, int(), real()]),
    begin
        Json = #{<<"prop_r2j_integer_min_max_type">> => Val},
        Rec = #prop_test_rec{prop_r2j_integer_min_max_type = Val},
        Expected = if
            is_integer(Val), -100 =< Val, Val =< 100 ->
                {ok, Rec};
            true ->
                {ok, Rec, [prop_r2j_integer_min_max_type]}
        end,
        Got = prop_test_rec:from_json(Json),
        Trans = fun(J) ->
            maps:filter(fun(K, _) -> K =:= prop_r2j_integer_min_max_type end, J)
        end,
        case Got of
            Expected ->
                try prop_test_rec:to_json(Rec, [Trans]) of
                    MaybeGood when is_integer(Val), -100 =< Val, Val =< 100 ->
                        jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(MaybeGood,[]);
                    NotGood ->
                        ?debugFmt("expected a boom due to ~p but got ~p", [Val, NotGood]),
                        false
                catch
                    error:{badarg, {prop_r2j_integer_min_max_type, Val, {specific, [undefined, {r2j_type, integer, [-100, 100]}]}}} when not is_integer(Val); Val < -100; 100 < Val ->
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
    ?FORALL(Val, list(oneof([<<"bin">>, int(), real()])),
    begin
        Json = #{<<"prop_r2j_integer_min_max_listed">> => Val},
        Rec = #prop_test_rec{prop_r2j_integer_min_max_listed = Val},
        FoldFun = fun
            (Int, _Index, Acc) when is_integer(Int), -100 =< Int, Int =< 100 ->
                Acc;
            (_Bad, Index, Acc) ->
                [Index | Acc]
        end,
        Expected = case fold_ind(FoldFun, [], Val) of
            [] ->
                {ok, Rec};
            Warns ->
                RevWarns = lists:reverse(Warns),
                TaggedWarns = [[prop_r2j_integer_min_max_listed, N] || N <- RevWarns],
                {ok, Rec, TaggedWarns}
        end,
        Got = prop_test_rec:from_json(Json),
        IsGoodValue = lists:all(fun(N) ->
            is_integer(N) andalso -100 =< N andalso N =< 100
        end, Val),
        Trans = fun(J) ->
            maps:filter(fun(Key,_) -> Key =:= prop_r2j_integer_min_max_listed end, J)
        end,
        case Got of
            Expected ->
                try prop_test_rec:to_json(Rec, [Trans]) of
                    Good when IsGoodValue ->
                        jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(Good,[]);
                    NotGood ->
                        ?debugFmt("expected a boom due to ~p but got ~p", [Val, NotGood]),
                        false
                catch
                    error:{badarg, {prop_r2j_integer_min_max_listed, Val, {specific, [undefined, {list, {specific, [{r2j_type, integer, [-100, 100]}]}}]}}} when not IsGoodValue ->
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
    ?FORALL({X, Y} = RecVal, {int(), int()}, begin
        Json = #{<<"type_translation">> => #{<<"x">> => X, <<"y">> => Y}},
        Rec = #prop_test_rec{type_translation = RecVal},
        Got = prop_test_rec:from_json(Json),
        FilterToOnlyTranslation = fun(J) ->
            maps:filter(fun(Key, _) -> Key =:= type_translation end, J)
        end,
        {ok, Rec} == Got andalso jsx_to_json:to_json(Json,[]) == jsx_to_json:to_json(prop_test_rec:to_json(Rec, [FilterToOnlyTranslation]),[])
    end).

fold_ind(Fun, InitAcc, List) ->
    {OutAcc, _} = lists:foldl(fun(Elem, {Acc, Ind}) ->
        NewAcc = Fun(Elem, Ind, Acc),
        {NewAcc, Ind + 1}
    end, {InitAcc, 1}, List),
    OutAcc.

point({X,Y}) ->
    {ok, #{x => X, y => Y}};

point(#{<<"x">> := X, <<"y">> := Y}) when is_number(X), is_number(Y) ->
    {ok, {X, Y}};

point(_) ->
    error.

