-module(mapreduce).
-export([test/0, mapreduce/5, mapreduce_seq/3, reduce_seq/2]).

test() ->
    Mapper = fun (_Key, Text) ->
		     [{Word, 1} || Word <- Text]
	     end,
    Reducer = fun (Word, Counts) ->
		      [{Word, lists:sum(Counts)}]
	      end,
    mapreduce(Mapper, 2, Reducer, 10, [{a, ["hello", "world", "hello", "text"]}, {b, ["world", "a", "b", "text"]}]).
		      

mapreduce_seq(Mapper, Reducer, Input) ->       
    Mapped = [{K2,V2} || {K,V} <- Input, {K2,V2} <- Mapper(K,V)],    
    reduce_seq(Reducer, Mapped).

reduce_seq(Reduce,KVs) -> 
    [KV || {K,Vs} <- groupkeys(lists:sort(KVs)), KV <- Reduce(K,Vs)]. 

%% INPUT:  [{K1, V1}, {K1, V2}, {K2, V3}]
%% OUTPUT: [{K1, [V1, V2]}, {K2, [V3]}]
groupkeys([]) ->
    [];
groupkeys([{K, V}|Rest]) ->
    groupkeys(K, [V], Rest).

groupkeys(K, Vs, [{K, V}|Rest]) ->
    groupkeys(K, [V|Vs], Rest);
groupkeys(K, Vs, Rest) ->
    [{K, lists:reverse(Vs)}|groupkeys(Rest)].

%% INPUT: [a,b,c,d], 2
%% OUTPUT: [[a,b], [c,d]]
%% INPUT: [a, b], 2
%% OUTPUT: [[],[], [a], [b]]
partition(N, List) ->
    partition(N, List, length(List)).

partition(1, List, _) ->
    [List];
partition(N, List, Len) ->
    {Prefix, Suffix} = lists:split(Len div N, List),
    [Prefix | partition(N - 1, Suffix, Len - (Len div N))].

mapreduce(Mapper, MapperCount, Reducer, ReducerCount, Input) ->
    Self = self(),
    Ref = make_ref(),
    Partitions = partition(MapperCount, Input),

    ReducerPids = loop_reducer_spawn(Self, Ref, Reducer, ReducerCount),

    MapperPids = [spawn_mapper(Self, Ref, Mapper, ReducerCount, Part, ReducerPids) || Part <- Partitions],
    [receive {map, {Pid, Ref}} -> ready end || Pid <- MapperPids],
    [RPid ! {ready, {Self, Ref}} || RPid <- ReducerPids],

    %io:fwrite("Waiting on reducers ~n", []),
    
    Output = [receive
		  {reduce, {Pid, Ref, Data}} ->
		      Data
	      end || Pid <- ReducerPids],

    %% Flatten the output from the reducers
    %% sort becouse it looks nice :-)
    lists:sort(lists:flatten(Output)).

%% INPUT: [{DataKey, [DataValue1, ..., DataValueN]}]
spawn_mapper(Master, Ref, Mapper, ReducerCount, Data, ReducerPids) ->
    spawn_link(fun () ->
		       %% phash: hash erlang term to `Reducers` bins.
		       %% In this case we tag each value with the same
		       %% value so that they will be processed by the
		       %% same reducer.
		       Map = [{erlang:phash2(MapKey, ReducerCount), {MapKey, MapValue}} ||
				 %% For each element in Data
				 {DataKey, DataValue} <- Data, 
				 %% Apply the Mapper to the key and value
				 %% and iterate over those
				 {MapKey, MapValue} <- Mapper(DataKey, DataValue)],
                
                Sorted = lists:sort(Map),
                Grouped = groupkeys(Sorted),
                Keyed = lists:map(fun({Key, Inner}) -> {lists:nth(Key, ReducerPids), Inner} end, Grouped), 
                
               [RPid ! {map, {Ref, Group}} || {RPid, Group} <- Keyed], % We do this before waking up the reducers to create a definite end point upon receiving
		       Master ! {map, {self(), Ref}}
               
	       end).

loop_reducer_spawn(Master, Ref, Reducer, 1, Acc) ->
    [spawn_reducer(Master, Ref, Reducer) | Acc];
loop_reducer_spawn(Master, Ref, Reducer, N, Acc) ->
    loop_reducer_spawn(Master, Ref, Reducer, N - 1, [spawn_reducer(Master, Ref, Reducer) | Acc]).
loop_reducer_spawn(Master, Ref, Reducer, N) ->
    loop_reducer_spawn(Master, Ref, Reducer, N - 1, [spawn_reducer(Master, Ref, Reducer)]).

spawn_reducer(Master, Ref, Reducer) ->
    spawn_link(fun () ->
               receive
                    {ready, {Master, Ref}} ->
                        reducer_proc(Master, Ref, Reducer)
                end
	       end).
		       		       
    
reducer_proc(Master, Ref, Reducer) ->
    case reducer_retreive(Ref) of
        [] -> Master ! {reduce, {self(), Ref, []}};
        Chunk ->  Reduce = [KV || {K,Vs} <- groupkeys(lists:sort(Chunk)),
		KV <- Reducer(K,Vs)],
		Master ! {reduce, {self(), Ref, Reduce}}
    end.


reducer_retreive(Ref, Acc) ->
        receive 
        {map, {Ref, Data}} -> 
            reducer_retreive(Ref, Acc ++ Data)
        after 0 -> Acc
    end.

reducer_retreive(Ref) ->
    receive 
        {map, {Ref, Data}} ->
            reducer_retreive(Ref, Data)
        after 0 -> [] % This can be done because every map will have sent their data by the time this runs, meaning if we hit this, we reached the end of the data.
    end.