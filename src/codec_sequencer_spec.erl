-module(codec_sequencer_spec).


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% API
-export([loop/1]).
-export([group/1]).
-export([object/3, object/4]).
-export([record/3, record/4]).
-export([map/2, map/3, map/4]).
-export([list/2, list/3]).
-export([tuple/2, tuple/3]).
-export([codec/2, codec/3]).
-export([option/2]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loop(ItemSpec) ->
    {loop, undefined, undefined, undefined, ItemSpec}.

group(SpecList) when is_list(SpecList) ->
    {group, undefined, undefined, undefined, SpecList}.

object(Necessity, ObjectSpec, SpecList) ->
    {object, Necessity, ObjectSpec, undefined, SpecList}.

object(Necessity, ObjectSpec, StorageParams, SpecList) ->
    {object, Necessity, ObjectSpec, StorageParams, SpecList}.

record(Necessity, Template, SpecList) ->
    {object, Necessity, {record, Template}, undefined, SpecList}.

record(Necessity, Template, StorageParams, SpecList) ->
    {object, Necessity, {record, Template}, StorageParams, SpecList}.

map(Necessity, SpecList) ->
    {object, Necessity, {map, #{}}, undefined, SpecList}.

map(Necessity, Template, SpecList) ->
    {object, Necessity, {map, Template}, undefined, SpecList}.

map(Necessity, Template, StorageParams, SpecList) ->
    {object, Necessity, {map, Template}, StorageParams, SpecList}.

list(Necessity, SpecList) ->
    {object, Necessity, list, undefined, SpecList}.

list(Necessity, StorageParams, SpecList) ->
    {object, Necessity, list, StorageParams, SpecList}.

tuple(Necessity, SpecList) ->
    {object, Necessity, tuple, undefined, SpecList}.

tuple(Necessity, StorageParams, SpecList) ->
    {object, Necessity, tuple, StorageParams, SpecList}.

codec(Necessity, CodecTag) ->
    {codec, Necessity, CodecTag, undefined, undefined}.

codec(Necessity, CodecTag, StorageParams) ->
    {codec, Necessity, CodecTag, StorageParams, undefined}.

option(Necessity, OptionsSpecs) ->
    {option, Necessity, undefined, undefined, OptionsSpecs}.
