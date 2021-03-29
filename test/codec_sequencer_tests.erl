-module(codec_sequencer_tests).

%%% INCLUDES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").


%%% IMPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-import(codec_sequencer_spec, [loop/1]).
-import(codec_sequencer_spec, [group/1]).
-import(codec_sequencer_spec, [object/3, object/4]).
-import(codec_sequencer_spec, [record/3, record/4]).
-import(codec_sequencer_spec, [map/2, map/3, map/4]).
-import(codec_sequencer_spec, [list/2, list/3]).
-import(codec_sequencer_spec, [tuple/2, tuple/3]).
-import(codec_sequencer_spec, [codec/2, codec/3]).
-import(codec_sequencer_spec, [option/2]).


%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(dummy, {foo, bar, boz, buz}).
-record(mock, {a, b, c, d}).


%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(assertEncDec(SPEC, OPTS, DATA), (fun(__Spec, __Opts, __Data) ->
    EncRes = codec_sequencer:encode(__Spec, __Data, __Opts),
    ?assertMatch({ok, _, _, _}, EncRes),
    {ok, EncData, EncSize, _} = EncRes,
    ?assertEqual(byte_size(EncData), EncSize),
    DecRes = codec_sequencer:decode(__Spec, EncData, __Opts),
    ?assertMatch({ok, _, <<>>, _}, DecRes),
    {ok, DecData, _, _} = DecRes,
    ?assertEqual(DecData, __Data)
end)(SPEC, OPTS, DATA)).
-define(_assertEncDec(SPEC, OPTS, DATA),
    ?_test(?assertEncDec(SPEC, OPTS, DATA))).
-define(_trace(FUN), case (FUN) of
    {__Tag, __Fun} when is_function(__Fun) ->
        {__Tag, fun() ->
            dbg:tracer(),
            dbg:p(all, c),
            dbg:tpl(codec_sequencer, x),
            try __Fun() after dbg:stop_clear() end
         end}
end).


%%% TEST CASES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_codec_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    OptBoolSpec = codec(optional, bool),
    OptCharSpec = codec(optional, char),
    ReqBoolSpec = codec(required, bool),
    ReqCharSpec = codec(required, char),

    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(OptBoolSpec, <<>>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(OptBoolSpec, <<0>>, Opts)),
        ?_assertMatch({ok, true, <<"bf">>, Ctx},
            codec_sequencer:decode(OptBoolSpec, <<"btbf">>, Opts)),
        ?_assertMatch({ok, $X, <<0>>, Ctx},
            codec_sequencer:decode(OptCharSpec, <<"cX", 0>>, Opts)),

        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(ReqBoolSpec, <<>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(ReqBoolSpec, <<0>>, Opts)),
        ?_assertMatch({ok, true, <<"bf">>, Ctx},
            codec_sequencer:decode(ReqBoolSpec, <<"btbf">>, Opts)),
        ?_assertMatch({ok, $X, <<0>>, Ctx},
            codec_sequencer:decode(ReqCharSpec, <<"cX", 0>>, Opts)),

        ?_assertMatch({not_found, Ctx},
            codec_sequencer:encode(OptBoolSpec, [], Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:encode(OptBoolSpec, 0, Opts)),
        ?_assertMatch({ok, <<"bt">>, 2, Ctx},
            codec_sequencer:encode(OptBoolSpec, true, Opts)),
        ?_assertMatch({ok, <<"cX">>, 2, Ctx},
            codec_sequencer:encode(OptCharSpec, $X, Opts)),

        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:encode(ReqBoolSpec, [], Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:encode(ReqBoolSpec, 0, Opts)),
        ?_assertMatch({ok, <<"bt">>, 2, Ctx},
            codec_sequencer:encode(ReqBoolSpec, true, Opts)),
        ?_assertMatch({ok, <<"cX">>, 2, Ctx},
            codec_sequencer:encode(ReqCharSpec, $X, Opts)),

        ?_assertEncDec(OptBoolSpec, Opts, true),
        ?_assertEncDec(OptBoolSpec, Opts, false),
        ?_assertEncDec(OptCharSpec, Opts, $X),
        ?_assertEncDec(OptCharSpec, Opts, 0),
        ?_assertEncDec(ReqBoolSpec, Opts, true),
        ?_assertEncDec(ReqBoolSpec, Opts, false),
        ?_assertEncDec(ReqCharSpec, Opts, $X),
        ?_assertEncDec(ReqCharSpec, Opts, 0)
    ].

single_codec_with_custom_exception_handling_test_() ->
    Ctx = make_ref(),
    Ctx2 = make_ref(),
    Ctx3 = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary},
        {mandatory_handler, fun(C, _Tag) ->
            ?assertMatch(Ctx, C),
            Ctx2
        end},
        {error_handler, fun(C, Error) ->
            ?assertMatch(Ctx2, C),
            ?assertMatch({mandatory_error, _}, Error),
            throw({my_custom_error, Ctx3})
        end}
    ],

    OptBoolSpec = codec(optional, bool),
    ReqBoolSpec = codec(required, bool),
    
    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(OptBoolSpec, <<>>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(OptBoolSpec, <<0>>, Opts)),

        ?_assertThrow({my_custom_error, Ctx3},
            codec_sequencer:decode(ReqBoolSpec, <<>>, Opts)),
        ?_assertThrow({my_custom_error, Ctx3},
            codec_sequencer:decode(ReqBoolSpec, <<0>>, Opts)),

        ?_assertMatch({not_found, Ctx},
            codec_sequencer:encode(OptBoolSpec, [], Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:encode(OptBoolSpec, 0, Opts)),

        ?_assertThrow({my_custom_error, Ctx3},
            codec_sequencer:encode(ReqBoolSpec, [], Opts)),
        ?_assertThrow({my_custom_error, Ctx3},
            codec_sequencer:encode(ReqBoolSpec, 0, Opts))
    ].

single_record_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],
    Spec = record(required, #dummy{}, [
        codec(required, bool, #dummy.foo),
        codec(optional, bool, #dummy.buz),
        codec(required, char, #dummy.bar)
    ]),

    [
        ?_assertEqual({ok, #dummy{foo = true, bar = $X, buz = false}, <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btbfcX", 0>>, Opts)),
        ?_assertEqual({ok, #dummy{foo = true, bar = $X, buz = undefined}, <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btcX", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(Spec, <<"cXcY">>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec, <<"btbf">>, Opts)),

        ?_assertEqual({ok, <<"btbfcX">>, 6, Ctx}, codec_sequencer:encode(Spec,
            #dummy{foo = true, bar = $X, buz = false}, Opts)),
        ?_assertEqual({ok, <<"btcX">>, 4, Ctx}, codec_sequencer:encode(Spec,
            #dummy{foo = true, bar = $X, buz = undefined}, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:encode(Spec, #dummy{}, Opts)),

        ?_assertEncDec(Spec, Opts, #dummy{foo = true, bar = $X, buz = false}),
        ?_assertEncDec(Spec, Opts, #dummy{foo = true, bar = $X})
    ].

single_map_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],
    Spec = map(required, #{}, [
        codec(required, bool, foo),
        codec(optional, bool, buz),
        codec(required, char, bar)
    ]),

    [
        ?_assertEqual({ok, #{foo => true, bar => $X, buz => false}, <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btbfcX", 0>>, Opts)),
        ?_assertEqual({ok, #{foo => true, bar => $X}, <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btcX", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec, <<"btbf">>, Opts)),

        ?_assertEqual({ok, <<"btbfcX">>, 6, Ctx}, codec_sequencer:encode(Spec,
            #{foo => true, bar => $X, buz => false}, Opts)),
        ?_assertEqual({ok, <<"btcX">>, 4, Ctx}, codec_sequencer:encode(Spec,
            #{foo => true, bar => $X}, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:encode(Spec, #{}, Opts)),

        ?_assertEncDec(Spec, Opts, #{foo => true, bar => $X, buz => false}),
        ?_assertEncDec(Spec, Opts, #{foo => true, bar => $X})
    ].

single_list_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],
    Spec = list(required, [
        codec(required, bool),
        codec(optional, bool),
        codec(required, char)
    ]),

    [
        ?_assertEqual({ok, [true, false, $X], <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btbfcX", 0>>, Opts)),
        ?_assertEqual({ok, [true, $X], <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btcX", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec, <<"btbf">>, Opts)),

        ?_assertEqual({ok, <<"btbfcX">>, 6, Ctx}, codec_sequencer:encode(Spec,
            [true, false, $X], Opts)),
        ?_assertEqual({ok, <<"btcX">>, 4, Ctx}, codec_sequencer:encode(Spec,
            [true, $X], Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:encode(Spec, [], Opts)),

        ?_assertEncDec(Spec, Opts, [true, false, $X]),
        ?_assertEncDec(Spec, Opts, [true, $X])
    ].

single_tuple_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],
    Spec = tuple(required, [
        codec(required, bool),
        codec(optional, bool),
        codec(required, char)
    ]),

    [
        ?_assertEqual({ok, {true, false, $X}, <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btbfcX", 0>>, Opts)),
        ?_assertEqual({ok, {true, $X}, <<0>>, Ctx},
            codec_sequencer:decode(Spec, <<"btcX", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec, <<"btbf">>, Opts)),

        ?_assertEqual({ok, <<"btbfcX">>, 6, Ctx}, codec_sequencer:encode(Spec,
            {true, false, $X}, Opts)),
        ?_assertEqual({ok, <<"btcX">>, 4, Ctx}, codec_sequencer:encode(Spec,
            {true, $X}, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:encode(Spec, {}, Opts)),

        ?_assertEncDec(Spec, Opts, {true, false, $X}),
        ?_assertEncDec(Spec, Opts, {true, $X})
    ].

simple_implicit_list_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    AllOptSpec = [
        codec(optional, bool),
        codec(optional, bool),
        codec(optional, char),
        codec(optional, char),
        codec(optional, bool),
        codec(optional, char)
    ],
    SomeReqSpec = [
        codec(required, bool),
        codec(optional, bool),
        codec(required, char),
        codec(optional, char),
        codec(required, bool),
        codec(optional, char)
    ],

    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(AllOptSpec, <<>>, Opts)),
        ?_assertEqual({ok, [true], <<0>>, Ctx},
            codec_sequencer:decode(AllOptSpec, <<"bt", 0>>, Opts)),
        ?_assertEqual({ok, [true, true, true], <<"bt">>, Ctx},
            codec_sequencer:decode(AllOptSpec, <<"btbtbtbt">>, Opts)),
        ?_assertEqual({ok, [true, $X, true, $X], <<"bt">>, Ctx},
            codec_sequencer:decode(AllOptSpec, <<"btcXbtcXbt">>, Opts)),
        ?_assertEqual({ok, [$X, true, $X], <<"cX">>, Ctx},
            codec_sequencer:decode(AllOptSpec, <<"cXbtcXcX">>, Opts)),

        ?_assertEncDec(AllOptSpec, Opts, [true]),
        ?_assertEncDec(AllOptSpec, Opts, [$X]),
        ?_assertEncDec(AllOptSpec, Opts, [true, false, true]),
        ?_assertEncDec(AllOptSpec, Opts, [$X, $Y, $Z]),
        ?_assertEncDec(AllOptSpec, Opts, [true, false, $X, $Y, true, $Z]),
        ?_assertEncDec(AllOptSpec, Opts, [true, $X, false, $Z]),
        ?_assertEncDec(AllOptSpec, Opts, [true, $X, $Z]),

        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<"bt", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<"cX", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<"btcX", 0>>, Opts)),
        ?_assertEqual({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<"btcXcX", 0>>, Opts)),
        ?_assertEqual({ok, [true, $X, false], <<>>, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<"btcXbf">>, Opts)),
        ?_assertEqual({ok, [true, false, $X, $Y, true], <<>>, Ctx},
            codec_sequencer:decode(SomeReqSpec, <<"btbfcXcYbt">>, Opts)),

        ?_assertEncDec(SomeReqSpec, Opts, [true, $X, false]),
        ?_assertEncDec(SomeReqSpec, Opts, [true, false, $X, true]),
        ?_assertEncDec(SomeReqSpec, Opts, [true, $X, $Y, false]),
        ?_assertEncDec(SomeReqSpec, Opts, [true, $X, false, $Y]),
        ?_assertEncDec(SomeReqSpec, Opts, [true, false, $X, $Y, false, $Z])
    ].

simple_option_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    OptSpec = option(optional, [
        codec(optional, bool),
        codec(required, char)
    ]),
    ReqSpec = option(required, [
        codec(optional, bool),
        codec(required, char)
    ]),

    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(OptSpec, <<>>, Opts)),
        ?_assertEqual({ok, true, <<"bf">>, Ctx},
            codec_sequencer:decode(OptSpec, <<"btbf">>, Opts)),
        ?_assertEqual({ok, $X, <<"cY">>, Ctx},
            codec_sequencer:decode(OptSpec, <<"cXcY">>, Opts)),

        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(ReqSpec, <<>>, Opts)),
        ?_assertEqual({ok, true, <<"bf">>, Ctx},
            codec_sequencer:decode(ReqSpec, <<"btbf">>, Opts)),
        ?_assertEqual({ok, $X, <<"cY">>, Ctx},
            codec_sequencer:decode(ReqSpec, <<"cXcY">>, Opts)),

        ?_assertMatch({not_found, Ctx},
            codec_sequencer:encode(OptSpec, undefined, Opts)),
        ?_assertEqual({ok, <<"bt">>, 2, Ctx},
            codec_sequencer:encode(OptSpec, true, Opts)),
        ?_assertEqual({ok, <<"cX">>, 2, Ctx},
            codec_sequencer:encode(OptSpec, $X, Opts)),

        ?_assertEqual({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:encode(ReqSpec, undefined, Opts)),
        ?_assertEqual({ok, <<"bt">>, 2, Ctx},
            codec_sequencer:encode(ReqSpec, true, Opts)),
        ?_assertEqual({ok, <<"cX">>, 2, Ctx},
            codec_sequencer:encode(ReqSpec, $X, Opts)),

        ?_assertEncDec(OptSpec, Opts, true),
        ?_assertEncDec(OptSpec, Opts, false),
        ?_assertEncDec(OptSpec, Opts, $X),
        ?_assertEncDec(OptSpec, Opts, $Y),
        ?_assertEncDec(ReqSpec, Opts, true),
        ?_assertEncDec(ReqSpec, Opts, false),
        ?_assertEncDec(ReqSpec, Opts, $X),
        ?_assertEncDec(ReqSpec, Opts, $Y)
    ].

optional_ojects_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    ListSpec = option(optional, [
        list(required, [
            codec(required, bool),
            codec(required, char),
            codec(required, char)
        ]),
        list(required, [
            codec(required, bool),
            codec(optional, char),
            codec(required, bool)
        ]),
        list(required, [
            codec(required, char),
            codec(optional, char)
        ])      
    ]),

    RecSpec = option(optional, [
        record(required, #dummy{}, [
            codec(required, bool, #dummy.foo),
            codec(required, char, #dummy.bar),
            codec(required, char, #dummy.buz)
        ]),
        record(required, #dummy{}, [
            codec(required, bool, #dummy.foo),
            codec(optional, char, #dummy.bar),
            codec(required, bool, #dummy.buz)
        ]),
        record(required, #mock{}, [
            codec(required, char, #mock.a),
            codec(optional, char, #mock.b)
        ])      
    ]),

    MixedSpec = option(optional, [
        map(required, [
            codec(required, bool, foo),
            codec(required, char, bar),
            codec(required, char, buz)
        ]),
        tuple(required, [
            codec(required, bool),
            codec(optional, char),
            codec(required, bool)
        ]),
        record(required, #mock{}, [
            codec(required, char, #mock.a),
            codec(optional, char, #mock.b)
        ])      
    ]),
    
    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(ListSpec, <<>>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(ListSpec, <<"bt">>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(ListSpec, <<"btcX">>, Opts)),

        % The first option has higher priorityy
        ?_assertEqual({ok, [true, $X, $Y], <<"bf">>, Ctx},
            codec_sequencer:decode(ListSpec, <<"btcXcYbf">>, Opts)),
        ?_assertEqual({ok, [true, false], <<"cX">>, Ctx},
            codec_sequencer:decode(ListSpec, <<"btbfcX">>, Opts)),        
        ?_assertEqual({ok, [true, $X, false], <<"cY">>, Ctx},
            codec_sequencer:decode(ListSpec, <<"btcXbfcY">>, Opts)),
        ?_assertEqual({ok, [$X], <<"bt">>, Ctx},
            codec_sequencer:decode(ListSpec, <<"cXbt">>, Opts)),
        ?_assertEqual({ok, [$X, $Y], <<"btbf">>, Ctx},
            codec_sequencer:decode(ListSpec, <<"cXcYbtbf">>, Opts)),

        ?_assertEncDec(ListSpec, Opts, [true, $X, $Y]),
        ?_assertEncDec(ListSpec, Opts, [true, false]),
        ?_assertEncDec(ListSpec, Opts, [true, $X, false]),
        ?_assertEncDec(ListSpec, Opts, [$X]),
        ?_assertEncDec(ListSpec, Opts, [$X, $Y]),

        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(RecSpec, <<>>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(RecSpec, <<"bt">>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(RecSpec, <<"btcX">>, Opts)),

        ?_assertEqual({ok, #dummy{foo = true, bar = $X, buz = $Y},
                       <<"bf">>, Ctx},
            codec_sequencer:decode(RecSpec, <<"btcXcYbf">>, Opts)),
        ?_assertEqual({ok, #dummy{foo = true, buz = false},
                       <<"cX">>, Ctx},
            codec_sequencer:decode(RecSpec, <<"btbfcX">>, Opts)),     
        ?_assertEqual({ok, #dummy{foo = true, bar = $X, buz = false},
                      <<"cY">>, Ctx},
            codec_sequencer:decode(RecSpec, <<"btcXbfcY">>, Opts)),
        ?_assertEqual({ok, #mock{a = $X}, <<"bt">>, Ctx},
            codec_sequencer:decode(RecSpec, <<"cXbt">>, Opts)),
        ?_assertEqual({ok, #mock{a = $X, b = $Y}, <<"btbf">>, Ctx},
            codec_sequencer:decode(RecSpec, <<"cXcYbtbf">>, Opts)),

        ?_assertEncDec(RecSpec, Opts,
            #dummy{foo = true, bar = $X, buz = $Y}),
        ?_assertEncDec(RecSpec, Opts,
            #dummy{foo = true, buz = false}),
        ?_assertEncDec(RecSpec, Opts,
            #dummy{foo = true, bar = $X, buz = false}),
        ?_assertEncDec(RecSpec, Opts,
            #mock{a = $X}),
        ?_assertEncDec(RecSpec, Opts,
            #mock{a = $X, b = $Y}),

        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(MixedSpec, <<>>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(MixedSpec, <<"bt">>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(MixedSpec, <<"btcX">>, Opts)),

        ?_assertEqual({ok, #{foo => true, bar => $X, buz => $Y},
                       <<"bf">>, Ctx},
            codec_sequencer:decode(MixedSpec, <<"btcXcYbf">>, Opts)),
        ?_assertEqual({ok, {true, false},
                       <<"cX">>, Ctx},
            codec_sequencer:decode(MixedSpec, <<"btbfcX">>, Opts)),       
        ?_assertEqual({ok, {true, $X, false},
                      <<"cY">>, Ctx},
            codec_sequencer:decode(MixedSpec, <<"btcXbfcY">>, Opts)),
        ?_assertEqual({ok, #mock{a = $X}, <<"bt">>, Ctx},
            codec_sequencer:decode(MixedSpec, <<"cXbt">>, Opts)),
        ?_assertEqual({ok, #mock{a = $X, b = $Y}, <<"btbf">>, Ctx},
            codec_sequencer:decode(MixedSpec, <<"cXcYbtbf">>, Opts)),

        ?_assertEncDec(MixedSpec, Opts,
            #{foo => true, bar => $X, buz => $Y}),
        ?_assertEncDec(MixedSpec, Opts,
            {true, false}),
        ?_assertEncDec(MixedSpec, Opts,
            {true, $X, false}),
        ?_assertEncDec(MixedSpec, Opts,
            #mock{a = $X}),
        ?_assertEncDec(MixedSpec, Opts,
            #mock{a = $X, b = $Y})

    ].

simple_loop_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    CharSpec = [loop(codec(optional, char))],
    CompSpec = [loop(option(required, [
                    list(required, loop(codec(required, char))),
                    list(required, loop(codec(optional, bool)))
                ]))],

    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(CharSpec, <<>>, Opts)),
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(CharSpec, <<"btbfbtbf">>, Opts)),
        ?_assertEqual({ok, "abcdef", <<"bf">>, Ctx},
            codec_sequencer:decode(CharSpec, <<"cacbcccdcecfbf">>, Opts)),

        ?_assertEncDec(CharSpec, Opts, "a"),
        ?_assertEncDec(CharSpec, Opts, "abcdef"),

        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(CompSpec, <<>>, Opts)),
        ?_assertEqual({ok, [[true], "a", [false], "b", [true], "c"], <<>>, Ctx},
            codec_sequencer:decode(CompSpec, <<"btcabfcbbtcc">>, Opts)),
        ?_assertEqual({ok, [[true, false, true], "a",
                            [false, true], "bc",
                            [false], "def"], <<>>, Ctx},
            codec_sequencer:decode(CompSpec, <<"btbfbtcabfbtcbccbfcdcecf">>, Opts)),

        ?_assertEncDec(CompSpec, Opts, ["a"]),
        ?_assertEncDec(CompSpec, Opts, [[true]]),
        ?_assertEncDec(CompSpec, Opts, ["abc"]),
        ?_assertEncDec(CompSpec, Opts, [[true, false, true]]),
        ?_assertEncDec(CompSpec, Opts, [[true], "a", [false], "b"]),
        ?_assertEncDec(CompSpec, Opts, [[true], "abc",
                                        [false, true], "de",
                                        [false, true, false], "f"])
    ].  

simple_group_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    Spec1 = [
        codec(required, bool),
        group([
            codec(required, enum),
            codec(optional, bool)
        ]),
        codec(required, char)
    ],
    Spec2 = [
        codec(required, bool),
        group([
            codec(optional, bool),
            codec(required, enum)
        ]),
        codec(required, char)
    ],
    Spec3 = map(required, #{}, [
        codec(required, bool, a),
        group([
            codec(optional, char, b1),
            codec(optional, char, b2),
            codec(required, enum, b3)
        ]),
        codec(optional, char, c),
        codec(optional, char, d)
    ]),
    Spec4 = record(required, #mock{}, [
        codec(required, bool, #mock.a),
        group([
            list(optional, #mock.b, loop(
                codec(required, char)
            )),
            codec(required, enum, #mock.c)
        ]),
        list(optional, #mock.d, loop(
            codec(required, char)
        ))
    ]),

    [
        ?_assertMatch({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(Spec1, <<>>, Opts)),
        ?_assertMatch({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec1, <<"bt">>, Opts)),
        ?_assertMatch({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec1, <<"btbf">>, Opts)),
        ?_assertMatch({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec1, <<"btbfcX">>, Opts)),
        ?_assertMatch({ok, [true, $X], <<>>, Ctx},
            codec_sequencer:decode(Spec1, <<"btcX">>, Opts)),
        ?_assertMatch({ok, [true, toto, $X], <<>>, Ctx},
            codec_sequencer:decode(Spec1, <<"bte1cX">>, Opts)),
        ?_assertMatch({ok, [true, tata, false, $X], <<>>, Ctx},
            codec_sequencer:decode(Spec1, <<"bte2bfcX">>, Opts)),

        ?_assertEncDec(Spec1, Opts, [true, $X]),
        ?_assertEncDec(Spec1, Opts, [true, toto, $X]),
        ?_assertEncDec(Spec1, Opts, [true, toto, false, $X]),

        ?_assertMatch({error, {mandatory_error, {codec, bool}}, Ctx},
            codec_sequencer:decode(Spec2, <<>>, Opts)),
        ?_assertMatch({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec2, <<"bt">>, Opts)),
        % Returning {codec, enum} would be more meaningful, but too complicated
        ?_assertMatch({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec2, <<"btbf">>, Opts)),
        % Returning {codec, enum} would be more meaningful, but too complicated
        ?_assertMatch({error, {mandatory_error, {codec, char}}, Ctx},
            codec_sequencer:decode(Spec2, <<"btbfcX">>, Opts)),
        ?_assertMatch({ok, [true, $X], <<>>, Ctx},
            codec_sequencer:decode(Spec2, <<"btcX">>, Opts)),
        ?_assertMatch({ok, [true, toto, $X], <<>>, Ctx},
            codec_sequencer:decode(Spec2, <<"bte1cX">>, Opts)),
        ?_assertMatch({ok, [true, false, tata, $X], <<>>, Ctx},
            codec_sequencer:decode(Spec2, <<"btbfe2cX">>, Opts)),

        ?_assertEncDec(Spec2, Opts, [true, $X]),
        ?_assertEncDec(Spec2, Opts, [true, tata, $X]),
        ?_assertEncDec(Spec2, Opts, [true, false, tata, $X]),

        ?_assertEqual({ok, #{a => true, b3 => toto}, <<>>, Ctx},
            codec_sequencer:decode(Spec3, <<"bte1">>, Opts)),
        ?_assertEqual({ok, #{a => true, b1 => $X, b2 => $Y, b3 => toto}, <<>>, Ctx},
            codec_sequencer:decode(Spec3, <<"btcXcYe1">>, Opts)),
        ?_assertEqual({ok, #{a => true, b1 => $X, b3 => toto}, <<>>, Ctx},
            codec_sequencer:decode(Spec3, <<"btcXe1">>, Opts)),
        ?_assertEqual({ok, #{a => true, c => $X}, <<>>, Ctx},
            codec_sequencer:decode(Spec3, <<"btcX">>, Opts)),
        ?_assertEqual({ok, #{a => true, c => $X, d => $Y}, <<>>, Ctx},
            codec_sequencer:decode(Spec3, <<"btcXcY">>, Opts)),

        ?_assertEqual({ok, #mock{a = true, c = toto}, <<>>, Ctx},
            codec_sequencer:decode(Spec4, <<"bte1">>, Opts)),
        ?_assertEqual({ok, #mock{a = true, b = [$X, $Y], c = toto}, <<>>, Ctx},
            codec_sequencer:decode(Spec4, <<"btcXcYe1">>, Opts)),
        ?_assertEqual({ok, #mock{a = true, d = [$X, $Y]}, <<>>, Ctx},
            codec_sequencer:decode(Spec4, <<"btcXcY">>, Opts))
    ].

composed_case_1_test_() ->
    Ctx = make_ref(),
    Opts = [
        {context, Ctx},
        {decoder, fun dummy_decode/3},
        {encoder, fun dummy_encode/3},
        {output, binary}
    ],

    Spec = [
        loop(
            record(required, #mock{}, [
                codec(required, enum, #mock.a),
                list(optional, #mock.b,
                    loop(                    
                        record(required, #dummy{}, [
                            list(optional, #dummy.foo,
                                loop(codec(optional, char))
                            ),
                            codec(required, bool, #dummy.bar),
                            list(optional, #dummy.buz,
                                loop(codec(required, bool))
                            )
                        ])
                    )
                ),
                codec(optional, char, #mock.c)
            ])
        )
    ],

    [
        ?_assertMatch({not_found, Ctx},
            codec_sequencer:decode(Spec, <<"cXcY">>, Opts)),
        ?_assertEqual({ok, [#mock{a = toto}], <<>>, Ctx},
            codec_sequencer:decode(Spec, <<"e1">>, Opts)),
        ?_assertEqual({ok, [#mock{a = toto}, #mock{a = tata}], <<>>, Ctx},
            codec_sequencer:decode(Spec, <<"e1e2">>, Opts)),

        ?_assertEqual({ok, [#mock{a = toto,
                                  b = [#dummy{foo = "abc", bar = true}]}],
                       <<>>, Ctx},
            codec_sequencer:decode(Spec, <<"e1cacbccbt">>, Opts)),
        ?_assertEqual({ok, [#mock{a = toto,
                                  b = [#dummy{foo = "abc", bar = true},
                                       #dummy{foo = "def", bar = false}]}],
                       <<>>, Ctx},
            codec_sequencer:decode(Spec, <<"e1cacbccbtcdcecfbf">>, Opts)),

        ?_assertEncDec(Spec, Opts, [#mock{a = toto}]),
        ?_assertEncDec(Spec, Opts, [#mock{a = toto}, #mock{a = tata}]),
        ?_assertEncDec(Spec, Opts, [
            #mock{a = toto, b = [
                #dummy{foo = "abcde", bar = true},
                #dummy{foo = "fghij", bar = false}
            ]},
            #mock{a = tata, b = [
                #dummy{bar = true, buz = [true, false, true]}
            ]},
            #mock{a = tata, c = $Y, b = [
                #dummy{bar = false}
            ]}
        ])
    ].


%%% INTENAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dummy_decode(Ctx, bool, <<$b, $t, Rest/binary>>) ->
    {ok, true, Rest, Ctx};
dummy_decode(Ctx, bool, <<$b, $f, Rest/binary>>) ->
    {ok, false, Rest, Ctx};
dummy_decode(Ctx, bool, _) ->
    {not_found, Ctx};
dummy_decode(Ctx, char, <<$c, V:8, Rest/binary>>) ->
    {ok, V, Rest, Ctx};
dummy_decode(Ctx, char, _Data) ->
    {not_found, Ctx};
dummy_decode(Ctx, enum, <<$e, $1, Rest/binary>>) ->
    {ok, toto, Rest, Ctx};
dummy_decode(Ctx, enum, <<$e, $2, Rest/binary>>) ->
    {ok, tata, Rest, Ctx};
dummy_decode(Ctx, enum, _Data) ->
    {not_found, Ctx};
dummy_decode(Ctx, Tag, Data) ->
    throw({decoding_error, Tag, Data, Ctx}).

dummy_encode(Ctx, bool, true) ->
    {ok, <<$b, $t>>, 2, Ctx};
dummy_encode(Ctx, bool, false) ->
    {ok, [<<$b>>, <<$f>>], 2, Ctx};
dummy_encode(Ctx, bool, _V) ->
    {not_found, Ctx};
dummy_encode(Ctx, char, V) when is_integer(V), V < 256, V >= 0 ->
    {ok, <<$c, V:8>>, 2, Ctx};
dummy_encode(Ctx, char, _V) ->
    {not_found, Ctx};
dummy_encode(Ctx, enum, toto) ->
    {ok, [<<$e>>, <<$1>>], 2, Ctx};
dummy_encode(Ctx, enum, tata) ->
    {ok, <<$e, $2>>, 2, Ctx};
dummy_encode(Ctx, enum, _V) ->
    {not_found, Ctx};
dummy_encode(Ctx, Tag, Value) ->
    throw({encoding_error, Tag, Value, Ctx}).
