-module(codec_sequencer).

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([decode/2, decode/3]).
-export([encode/2, encode/3]).


%%% RECORDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(?MODULE, {
    % Caller-defined context
    context,
    stack = [],
    % Callbacks
    decoder_fun,
    encoder_fun,
    reset_error_fun,
    handle_mandatory_fun,
    recover_error_fun,
    handle_error_fun
}).


%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ST, ?MODULE).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Spec, Data) ->
    start_decoding(#?ST{}, Spec, Data).

decode(Spec, Data, Opts) when is_list(Opts) ->
    State = #?ST{
        context = proplists:get_value(context, Opts),
        decoder_fun = proplists:get_value(decoder, Opts),
        reset_error_fun = proplists:get_value(reset_error, Opts),
        handle_mandatory_fun = proplists:get_value(mandatory_handler, Opts),
        recover_error_fun = proplists:get_value(error_recovery, Opts),
        handle_error_fun = proplists:get_value(error_handler, Opts)
    },
    start_decoding(State, Spec, Data);
decode(Spec, Data, Opts) when is_map(Opts) ->
    State = #?ST{
        context = maps:get(context, Opts, undefined),
        decoder_fun = maps:get(decoder, Opts, undefined),
        reset_error_fun = maps:get(reset_error, Opts, undefined),
        handle_mandatory_fun = maps:get(mandatory_handler, Opts, undefined),
        recover_error_fun = maps:get(error_recovery, Opts, undefined),
        handle_error_fun = maps:get(error_handler, Opts, undefined)
    },
    start_decoding(State, Spec, Data).

encode(Spec, Data) ->
    start_encoding(#?ST{}, Spec, Data).

encode(Spec, Data, Opts) when is_list(Opts) ->
    State = #?ST{
        context = proplists:get_value(context, Opts),
        encoder_fun = proplists:get_value(encoder, Opts),
        reset_error_fun = proplists:get_value(reset_error, Opts),
        handle_mandatory_fun = proplists:get_value(mandatory_handler, Opts),
        recover_error_fun = proplists:get_value(error_recovery, Opts),
        handle_error_fun = proplists:get_value(error_handler, Opts)
    },
    OutputFormat = proplists:get_value(output, Opts, iodata),
    process_encoding_output(OutputFormat, start_encoding(State, Spec, Data));
encode(Spec, Data, Opts) when is_map(Opts) ->
    State = #?ST{
        context = maps:get(context, Opts, undefined),
        encoder_fun = maps:get(encoder, Opts, undefined),
        reset_error_fun = maps:get(reset_error, Opts, undefined),
        handle_mandatory_fun = maps:get(mandatory_handler, Opts, undefined),
        recover_error_fun = maps:get(error_recovery, Opts, undefined),
        handle_error_fun = maps:get(error_handler, Opts, undefined)
    },
    OutputFormat = maps:get(format, Opts, iodata),
    process_encoding_output(OutputFormat, start_encoding(State, Spec, Data)).


%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_stack(State, Spec) when is_list(Spec) ->
    push_object(State, list, undefined, []);
prepare_stack(State, _Spec) ->
    push_object(State, value, undefined, undefined).

prepare_stack(State, Spec, Data) when is_list(Spec), is_list(Data) ->
    push_object(State, list, undefined, Data);
prepare_stack(State, _Spec, Data) ->
    push_object(State, value, undefined, Data).


%-- Decoding Functions ---------------------------------------------------------

process_encoding_output(binary, {ok, Result, Size, Ctx}) ->
    {ok, iolist_to_binary(Result), Size, Ctx};
process_encoding_output(_, Result) ->
    Result.

start_decoding(State, [], Data) ->
    {ok, [], Data, State};
start_decoding(State, Spec, Data) ->
    case decode_specs(prepare_stack(State, Spec), Spec, Data) of
        {ok, _, RemData, #?ST{context = Ctx} = State2} ->
            case get_object(State2) of
                {ok, Result} -> {ok, Result, RemData, Ctx};
                not_found -> {not_found, Ctx}
            end;
        {not_found, _, #?ST{context = Ctx}} ->
            {not_found, Ctx};
        {error, _, Reason, State2} ->
            call_handle_error(State2, Reason)
    end.

decode_specs(State, SpecList, Data) ->
    decode_specs(State, SpecList, 0, Data).

decode_specs(State, [], Count, Data) ->
    {ok, Count, Data, State};
decode_specs(State, [Spec | RemSpecs], Count, Data) ->
    case decode_specs(State, Spec, Count, Data) of
        {not_found, _, State2} ->
            decode_specs(State2, RemSpecs, Count, Data);
        {ok, Count2, RemData, State2} ->
            decode_specs(State2, RemSpecs, Count2, RemData);
        {error, _Count, _Reason, _State2} = Result ->
            Result
    end;
decode_specs(State, Spec, Count, Data) ->
    case decode_spec(State, Spec, Data) of
        {not_found, _, _State2} = Result -> Result;
        {ok, RemData, State2} -> {ok, Count + 1, RemData, State2};
        {error, Reason, State2} -> {error, Count, Reason, State2}
    end.

decode_spec(State, {object, Necessity, ObjSpec, StoreParams, SpecList}, Data) ->
    decode_object(State, Necessity, ObjSpec, StoreParams, SpecList, Data);
decode_spec(State, {codec, Necessity, CodecTag, StoreParams, _}, Data) ->
    decode_codec(State, Necessity, CodecTag, StoreParams, Data);
decode_spec(State, {option, Necessity, _, _, SpecList}, Data) ->
    decode_option(State, Necessity, SpecList, Data);
decode_spec(State, {loop, _, _, _, Spec}, Data) ->
    decode_loop(State, Spec, Data);
decode_spec(State, {group, _, _, _, SpecList}, Data) ->
    decode_group(State, SpecList, Data);
decode_spec(_State, Spec, _Data) ->
    throw({invalid_spec, Spec}).

decode_codec(State, optional, CodecTag, StoreParams, Data) ->
    case call_decoder(State, CodecTag, Data) of
        {ok, DecData, RemData, State2} ->
            {ok, RemData, store(State2, StoreParams, DecData)};
        {not_found, State2} ->
            {not_found, CodecTag, State2};
        {error, _Reason, _State2} = Result ->
            Result
    end;
decode_codec(State, required, CodecTag, StoreParams, Data) ->
    case call_decoder(State, CodecTag, Data) of
        {ok, DecData, RemData, State2} ->
            {ok, RemData, store(State2, StoreParams, DecData)};
        {not_found, State2} ->
            {error, {mandatory_error, {codec, CodecTag}},
             call_handle_mandatory(State2, CodecTag)};
        {error, _Reason, _State2} = Result ->
            Result
    end;
decode_codec(_State, Necessity, _CodecTag, _StoreParams, _Data) ->
    throw({invalid_necessity, Necessity}).

decode_object(State, optional, ObjectSpec, StoreParams, SpecList, Data) ->
    State2 = create_object(State, ObjectSpec, StoreParams),
    case decode_specs(State2, SpecList, Data) of
        {ok, _, RemData, State3} ->
            case store_object(State3) of
                {not_defined, State4} ->
                    {not_found, undefined, State4};
                {ok, State4} ->
                    {ok, RemData, State4}
            end;
        {not_found, CodecTag, State3} ->
            {not_found, CodecTag, abort_object(State3)};
        {error, _, {mandatory_error, {codec, CodecTag}}, State3} ->
            {not_found, CodecTag,
             call_reset_error(abort_objects(State3, State2))};
        {error, _, {mandatory_error, _}, State3} ->
            {not_found, undefined,
             call_reset_error(abort_objects(State3, State2))};
        {error, _, Reason, State3} ->
            {error, Reason, abort_objects(State3, State2)}
    end;
decode_object(State, required, ObjectSpec, StoreParams, SpecList, Data) ->
    State2 = create_object(State, ObjectSpec, StoreParams),
    case decode_specs(State2, SpecList, Data) of
        {ok, _, RemData, State3} ->
            case store_object(State3) of
                {not_defined, State4} ->
                    {error, {mandatory_error, object}, State4};
                {ok, State4} ->
                    {ok, RemData, State4}
            end;
        {not_found, CodecTag, State3} ->
            %TODO: Keep the CodecTag around ?
            {error, {mandatory_error, {codec, CodecTag}}, abort_object(State3)};
        {error, _, Reason, State3} ->
            {error, Reason, abort_objects(State3, State2)}
    end;
decode_object(_State, Necessity, _ObjectSpec, _StoreParams, _SpecList, _Data) ->
    throw({invalid_necessity, Necessity}).

decode_option(State, Necessity, Specs, Data) ->
    decode_option(State, Necessity, Specs, undefined, Data).

decode_option(State, optional, [], {codec, CodecTag}, _Data) ->
    {not_found, CodecTag, State};
decode_option(State, optional, [], _, _Data) ->
    {not_found, undefined, State};
decode_option(State, required, [], undefined, _Data) ->
    {error, {mandatory_error, option}, State};
decode_option(State, required, [], Mandatory, _Data) ->
    {error, {mandatory_error, Mandatory}, State};
decode_option(_State, Necessity, [], _StoreParams, _Data) ->
    throw({invalid_necessity, Necessity});
decode_option(State, Necessity, [Spec | RemSpecs], Mandatory, Data) ->
    case decode_spec(State, Spec, Data) of
        {not_found, _, _State2} ->
            %FIXME: Maybe keep the context ?
            decode_option(State, Necessity, RemSpecs, Mandatory, Data);
        {error, {mandatory_error, Mandatory2}, _State2} ->
            %FIXME: Maybe keep the context ?
            Mandatory3 = if
                Mandatory =:= undefined -> Mandatory2;
                true -> Mandatory
            end,
            decode_option(call_reset_error(State), Necessity, RemSpecs,
                          Mandatory3, Data);
        {error, _Reason, _State2} = Result ->
            Result;
        {ok, _RemData, _State2} = Result ->
            Result
    end.

decode_loop(State, Spec, Data) ->
    decode_loop(State, Spec, 0, Data).

decode_loop(State, Spec, Counter, Data) ->
    case decode_spec(State, Spec, Data) of
        {not_found, _CodecTag, _State2} ->
            %FIXME: Maybe keep the context ?
            decode_loop_terminate(State, Counter, Data);
        {ok, RemData, State2} ->
            decode_loop(State2, Spec, Counter + 1, RemData);
        {error, {mandatory_error, _}, _State2} ->
            %FIXME: Maybe keep the context ?
            decode_loop_terminate(call_reset_error(State), Counter, Data);
        {error, _Reason, _State2} = Result ->
            Result
    end.

decode_loop_terminate(State, 0, _Data) ->
    {not_found, undefined, call_reset_error(State)};
decode_loop_terminate(State, _, Data) ->
    {ok, Data, State}.

decode_group(State, SpecList, Data) ->
    case decode_specs(State, SpecList, Data) of
        {ok, _, RemData, State2} ->
            {ok, RemData, State2};
        {not_found, CodecTag, State2} ->
            {not_found, CodecTag, reset_object(State2, State)};
        {error, _, {mandatory_error, {codec, CodecTag}}, State2} ->
            State3 = call_reset_error(reset_object(State2, State)),
            {not_found, CodecTag, State3};
        {error, _, {mandatory_error, _}, State2} ->
            State3 = call_reset_error(reset_object(State2, State)),
            {not_found, undefined, State3};
        {error, _, Reason, State2} ->
            {error, Reason, State2}
    end.


%-- Encoding Functions ---------------------------------------------------------

start_encoding(State, Spec, Data) ->
    case encode_specs(prepare_stack(State, Spec, Data), Spec, 0, []) of
        {ok, 0, _Buff, _Size, #?ST{context = Ctx}} ->
            {not_found, undefined, Ctx};
        {ok, _, Buff, Size, #?ST{context = Ctx, stack = [_]}} ->
            {ok, lists:reverse(Buff), Size, Ctx};
        {not_found, _, #?ST{context = Ctx}} ->
            {not_found, Ctx};
        {error, _, Reason, State2} ->
            call_handle_error(State2, Reason)
    end.

encode_specs(State, Specs, BuffSize, Buff) ->
    encode_specs(State, Specs, 0, BuffSize, Buff).

encode_specs(State, [], Count, BuffSize, Buff) ->
    {ok, Count, Buff, BuffSize, State};
encode_specs(State, [Spec | RemSpecs], Count, BuffSize, Buff) ->
    case encode_specs(State, Spec, Count, BuffSize, Buff) of
        {ok, Count2, NewBuff, NewBuffSize, State2} ->
            encode_specs(State2, RemSpecs, Count2, NewBuffSize, NewBuff);
        {not_found, _CodecTag, State2} ->
            encode_specs(State2, RemSpecs, Count, BuffSize, Buff);
        {error, _Count, _Reason, _State2} = Result ->
            Result
    end;
encode_specs(State, Spec, Count, BuffSize, Buff) ->
    case encode_spec(State, Spec, BuffSize, Buff) of
        {ok, NewBuff, NewBuffSize, State2} ->
            {ok, Count + 1, NewBuff, NewBuffSize, State2};
        {not_found, _CodecTag, _State2} = Result ->
            Result;
        {error, Reason, State2} ->
            {error, Count, Reason, State2}
    end.

encode_spec(State, {object, Necessity, ObjectSpec, StoreParams, SpecList},
            BuffSize, Buff) ->
    encode_object(State, Necessity, ObjectSpec, StoreParams, SpecList,
                  BuffSize, Buff);
encode_spec(State, {codec, Necessity, CodecTag, StoreParams, _},
            BuffSize, Buff) ->
    encode_codec(State, Necessity, CodecTag, StoreParams, BuffSize, Buff);
encode_spec(State, {option, Necessity, _, _, SpecList}, BuffSize, Buff) ->
    encode_option(State, Necessity, SpecList, BuffSize, Buff);
encode_spec(State, {loop, _, _, _, Spec}, BuffSize, Buff) ->
    encode_loop(State, Spec, BuffSize, Buff);
encode_spec(State, {group, _, _, _, SpecList}, BuffSize, Buff) ->
    encode_group(State, SpecList, BuffSize, Buff).

encode_codec(State, optional, CodecTag, StoreParams, BuffSize, Buff) ->
    case prepare_retrieve(State, StoreParams) of
        not_found ->
            {not_found, CodecTag, State};
        {ok, Value, Commit} ->
            case call_encoder(State, CodecTag, Value) of
                {ok, EncData, EncSize, State2} ->
                    {ok, [EncData | Buff], BuffSize + EncSize,
                     commit(State2, Commit)};
                {not_found, State2} ->
                    {not_found, CodecTag, State2};
                {error, _Reason, _State2} = Result ->
                    Result
            end
    end;
encode_codec(State, required, CodecTag, StoreParams, BuffSize, Buff) ->
    case prepare_retrieve(State, StoreParams) of
        not_found ->
            {error, {mandatory_error, {codec, CodecTag}},
             call_handle_mandatory(State, CodecTag)};
        {ok, Value, Commit} ->
            case call_encoder(State, CodecTag, Value) of
                {ok, EncData, EncSize, State2} ->
                    {ok, [EncData | Buff], BuffSize + EncSize,
                     commit(State2, Commit)};
                {not_found, State2} ->
                    {error, {mandatory_error, {codec, CodecTag}},
                     call_handle_mandatory(State2, CodecTag)};
                {error, _Reason, _State2} = Result ->
                    Result
            end
    end.

encode_object(State, optional, ObjectSpec, StoreParams, SpecList,
              BuffSize, Buff) ->
    case retrieve(State, StoreParams) of
        {not_found, State2} ->
            {not_found, undefined, State2};
        {ok, Value, State2} ->
            case select_object(State2, ObjectSpec, StoreParams, Value) of
                not_found ->
                    {not_found, undefined, State2};
                State3 ->
                    case encode_specs(State3, SpecList, BuffSize, Buff) of
                        {ok, _, NewBuff, NewBuffSize, State4} ->
                            {ok, NewBuff, NewBuffSize, abort_object(State4)};
                        {not_found, CodecTag, State4} ->
                            {not_found, CodecTag, abort_object(State4)};
                        {error, _, {mandatory_error, {codec, CodecTag}}, State4} ->
                            {not_found, CodecTag,
                             call_reset_error(abort_object(State4))};
                        {error, _, {mandatory_error, _}, State4} ->
                            {not_found, undefined,
                             call_reset_error(abort_object(State4))};
                        {error, _, Reason, State4} ->
                            {error, Reason, abort_objects(State4, State3)}
                    end
            end
    end;
encode_object(State, required, ObjectSpec, StoreParams, SpecList,
              BuffSize, Buff) ->
    case retrieve(State, StoreParams) of
        {not_found, State2} ->
            {error, {mandatory_error, object}, State2};
        {ok, Value, State2} ->
            case select_object(State2, ObjectSpec, StoreParams, Value) of
                not_found ->
                    {error, {mandatory_error, object}, abort_object(State2)};
                State3 ->
                    case encode_specs(State3, SpecList, BuffSize, Buff) of
                        {ok, _, NewBuff, NewBuffSize, State4} ->
                            {ok, NewBuff, NewBuffSize, abort_object(State4)};
                        {not_found, undefined, State4} ->
                            {error, {mandatory_error, object},
                             abort_object(State4)};
                        {not_found, CodecTag, State4} ->
                            {error, {mandatory_error, {codec, CodecTag}},
                             abort_object(State4)};
                        {error, _, Reason, State4} ->
                            {error, Reason, abort_objects(State4, State3)}
                    end
            end
    end.

encode_option(State, Necessity, Specs, BuffSize, Buff) ->
    encode_option(State, Necessity, Specs, undefined, BuffSize, Buff).

encode_option(State, optional, [], {codec, CodecTag}, _BuffSize, _Buff) ->
    {not_found, CodecTag, State};
encode_option(State, optional, [], _, _BuffSize, _Buff) ->
    {not_found, undefined, State};
encode_option(State, required, [], undefined, _BuffSize, _Buff) ->
    {error, {mandatory_error, option}, State};
encode_option(State, required, [], Mandatory, _BuffSize, _Buff) ->
    {error, {mandatory_error, Mandatory}, State};
encode_option(State, Necessity, [Spec | RemSpecs], Mandatory, BuffSize, Buff) ->
    case encode_spec(State, Spec, BuffSize, Buff) of
        {ok, _NewBuff, _NewBuffSize, _State2} = Result ->
            Result;
        {not_found, _, _State2} ->
            %FIXME: Maybe keep the context ?
            encode_option(State, Necessity, RemSpecs, Mandatory,
                          BuffSize, Buff);
        {error, {mandatory_error, Mandatory2}, _State2} ->
            %FIXME: Maybe keep the context ?
            Mandatory3 = if
                Mandatory =:= undefined -> Mandatory2;
                true -> Mandatory
            end,
            encode_option(call_reset_error(State), Necessity, RemSpecs,
                          Mandatory3, BuffSize, Buff);
        {error, _Reason, _State2} = Result ->
            Result
    end.

% We can only do that with list values for now, otherwise it would never ends
encode_loop(#?ST{stack = [{_, _, L, _} | _]} = State, Spec,
            BuffSize, Buff)
  when is_list(L) ->
    encode_loop(State, Spec, 0, BuffSize, Buff).

encode_loop(State, Spec, Counter, BuffSize, Buff) ->
    case encode_spec(State, Spec, BuffSize, Buff) of
        {not_found, _CodecTag, _State2} ->
            %FIXME: Maybe keep the context ?
            encode_loop_terminate(State, Counter, BuffSize, Buff);
        {ok, NewBuff, NewBuffSize, State2} ->
            encode_loop(State2, Spec, Counter + 1, NewBuffSize, NewBuff);
        {error, {mandatory_error, _}, _State2} ->
            %FIXME: Maybe keep the context ?
            encode_loop_terminate(call_reset_error(State), Counter,
                                  BuffSize, Buff);
        {error, _Reason, _State2} = Result ->
            Result
    end.

encode_loop_terminate(State, 0, _BuffSize, _Buff) ->
    {not_found, undefined, State};
encode_loop_terminate(State, _Counter, BuffSize, Buff) ->
    {ok, Buff, BuffSize, State}.

encode_group(State, SpecList, BuffSize, Buff) ->
    case encode_specs(State, SpecList, BuffSize, Buff) of
        {ok, _, NewBuff, NewBuffSize, State2} ->
            {ok, NewBuff, NewBuffSize, State2};
        {not_found, _CodecTag, _State2} = Result ->
            Result;
        {error, _, {mandatory_error, {codec, CodecTag}}, State2} ->
            {not_found, CodecTag, call_reset_error(State2)};
        {error, _, {mandatory_error, _}, State2} ->
            {not_found, undefined, call_reset_error(State2)};
        {error, _, Reason, State2} ->
            {error, Reason, State2}
    end.


%-- Object Handling Functions --------------------------------------------------

create_object(State, {record, Template}, StoreParams) ->
    push_object(State, record, StoreParams, Template);
create_object(State, {map, Template}, StoreParams) ->
    push_object(State, map, StoreParams, Template);
create_object(State, list, StoreParams) ->
    push_object(State, list, StoreParams, []);
create_object(State, tuple, StoreParams) ->
    push_object(State, tuple, StoreParams, []).

select_object(State, {record, Rec}, StoreParams, Obj)
    when is_tuple(Rec), is_tuple(Obj), element(1, Rec) =:= element(1, Obj) ->
    case {element(1, Rec), element(1, Obj)} of
        {RecName, RecName} -> push_object(State, record, StoreParams, Obj);
        _Other -> not_found
    end;
select_object(State, {map, _}, StoreParams, Obj) when is_map(Obj) ->
    push_object(State, map, StoreParams, Obj);
select_object(State, list, StoreParams, Obj) when is_list(Obj) ->
    push_object(State, list, StoreParams, Obj);
select_object(State, tuple, StoreParams, Obj) when is_tuple(Obj) ->
    push_object(State, tuple, StoreParams, tuple_to_list(Obj));
select_object(_State, _Type, _StoreParams, _Obj) ->
    not_found.

abort_object(State) ->
    {State2, _} = pop_object(State),
    State2.

% Aborts the current object of the reference state, and transplant the new
% stack to the new state.
abort_objects(State, StateRef) ->
    {#?ST{stack = NewStack}, _} = pop_object(StateRef),
     State#?ST{stack = NewStack}.

% Reset the current object to the reference state, used when an aborted
% operation may have change the object state, like a group that do not match
% the spec.
reset_object(State, #?ST{stack = RefStack}) ->
    State#?ST{stack = RefStack}.

store_object(State) ->
    case pop_object(State) of
        {State2, {_Type, _StoreParams, _Object, false}} ->
            {not_defined, State2};
        {State2, {Type, StoreParams, Object, true}} ->
            {ok, store(State2, StoreParams, postprocess_object(Type, Object))}
    end.

push_object(#?ST{stack = Stack} = State,
            Type, StoreParams, Object) ->
    State#?ST{
        stack = [{Type, StoreParams, Object, false} | Stack]
    }.

pop_object(#?ST{stack = [ObjInfo | NewStack]} = State) ->
    {State#?ST{stack = NewStack}, ObjInfo}.

get_object(#?ST{stack = [{T, _,  O, true} | _]}) ->
    {ok, postprocess_object(T, O)};
get_object(#?ST{stack = [{_, _,  _, false} | _]}) ->
    not_found.

postprocess_object(list, List) -> lists:reverse(List);
postprocess_object(tuple, List) -> list_to_tuple(lists:reverse(List));
postprocess_object(_Type, Object) -> Object.

store(#?ST{stack = [{Type, PParams, Obj, _} | Stack]} = State,
      Params, Value) ->
    Obj2 = store(Type, Params, Obj, Value),
    State#?ST{stack = [{Type, PParams, Obj2, true} | Stack]}.

store(value, undefined, undefined, Value) -> Value;
store(list, undefined, List, Value) when is_list(List) -> [Value | List];
store(tuple, undefined, List, Value) when is_list(List)-> [Value | List];
store(map, Key, Map, Value) when is_map(Map)-> maps:put(Key, Value, Map);
store(record, Index, Record, Value) when is_tuple(Record) ->
    setelement(Index, Record, Value);
store(Type, Param, _Obj, _Value) ->
    throw({codec_sequencer_error, {invalid_store_param, Type, Param}}).


%-- Retrieval Functions --------------------------------------------------------

prepare_retrieve(#?ST{stack = [{T, P, O, _} = E | _]}, Params) ->
    case retrieve(T, Params, O) of
        {ok, Value, O2} -> {ok, Value, {E, {T, P, O2, undefined}}};
        {ok, Value} -> {ok, Value, undefined};
        not_found -> not_found
    end.

commit(State, undefined) ->
    State;
commit(#?ST{stack = [E | Stack]} = State, {E, E2}) ->
    State#?ST{stack = [E2 | Stack]}.

retrieve(#?ST{stack = [{T, P, O, _} | S]} = State, Params) ->
    case retrieve(T, Params, O) of
        {ok, Value, O2} ->
            State2 = State#?ST{stack = [{T, P, O2, undefined} | S]},
            {ok, Value, State2};
        {ok, Value} ->
            {ok, Value, State};
        not_found ->
            {not_found, State}
    end.

retrieve(value, undefined, Value) ->
    {ok, Value};
retrieve(list, undefined, [Value | List]) ->
    {ok, Value, List};
retrieve(tuple, undefined, [Value | List]) ->
    {ok, Value, List};
retrieve(map, Key, Map) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> {ok, Value};
        error -> not_found
    end;
retrieve(record, Index, Rec)
  when is_tuple(Rec), Index > 1, Index =< size(Rec) ->
    {ok, element(Index, Rec)};
retrieve(_, _, _) ->
    not_found.


%-- Callback Functions ---------------------------------------------------------

call_decoder(#?ST{context = Ctx, decoder_fun = Fun} = State,
             Tag, Data) when is_function(Fun) ->
    try Fun(Ctx, Tag, Data) of
        {ok, DecData, RemData, Ctx2} ->
            {ok, DecData, RemData, State#?ST{context = Ctx2}};
        {not_found, Ctx2} ->
            {not_found, State#?ST{context = Ctx2}};
        {error, Reason, Ctx2} ->
            {error, Reason, State#?ST{context = Ctx2}}
    catch
        throw:Error:Stack ->
            case call_recover_error(State, Error, Stack) of
                {recover, State2} ->
                    {not_found, Tag, State2};
                {error, _Reason, _State2} = Result ->
                    Result
            end
    end.

call_encoder(#?ST{context = Ctx, encoder_fun = Fun} = State,
             Tag, Data) when is_function(Fun) ->
    try Fun(Ctx, Tag, Data) of
        {ok, EncData, Size, Ctx2} ->
            {ok, EncData, Size, State#?ST{context = Ctx2}};
        {not_found, Ctx2} ->
            {not_found, State#?ST{context = Ctx2}};
        {error, Reason, Ctx2} ->
            {error, Reason, State#?ST{context = Ctx2}}
    catch
        throw:Error:Stack ->
            case call_recover_error(State, Error, Stack) of
                {recover, State2} ->
                    {not_found, Tag, State2};
                {error, _Reason, _State2} = Result ->
                    Result
            end
    end.

call_reset_error(#?ST{context = Ctx, reset_error_fun = Fun} = State)
  when is_function(Fun) ->
    State#?ST{context = Fun(Ctx)};
call_reset_error(State) ->
    State.

call_handle_mandatory(#?ST{context = Ctx,
                                       handle_mandatory_fun = Fun} = State,
                      Tag) when is_function(Fun) ->
    State#?ST{context = Fun(Ctx, Tag)};
call_handle_mandatory(State, _Tag) ->
    State.

call_recover_error(#?ST{context = Ctx,
                                    recover_error_fun = Fun} = State,
                   Error, Stack) when is_function(Fun) ->
    case Fun(Ctx, Error) of
        {recover, Ctx2} ->
            {recover, State#?ST{context = Ctx2}};
        {error, Reason, Ctx2} ->
            {error, Reason, State#?ST{context = Ctx2}};
        {throw, Error2} ->
            erlang:raise(throw, Error2, Stack)
    end;
call_recover_error(_State, Error, Stack) ->
    erlang:raise(throw, Error, Stack).

call_handle_error(#?ST{context = Ctx, handle_error_fun = Fun},
                  Reason) when is_function(Fun) ->
    Fun(Ctx, Reason);
call_handle_error(#?ST{context = Ctx}, Reason) ->
    {error, Reason, Ctx}.
