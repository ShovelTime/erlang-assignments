-module(allocator).
-export([start/1, request/2, release/2]).

start(Resources) when is_map(Resources) ->
    spawn(fun() -> resource_loop( #{}, #{}, Resources) end).

resource_loop(Allocated, Waiting, Resources) ->
    receive
        {release, Src, ResourceKeys} ->
            case is_valid_release(Allocated, ResourceKeys, Src) of
                ok -> 
                    NewAlloc = maps:without(ResourceKeys, Allocated),
                    case select_next_candidate(NewAlloc, maps:iter(Waiting)) of
                        none -> resource_loop(NewAlloc, Waiting, Resources);
                        Candidate -> 
                            Owned = allocate_resources(Resources, ResourceKeys, Candidate),
                            resource_loop(maps:merge(NewAlloc, Owned), maps:remove(Candidate, Waiting), Resources)
                    end;
                _  -> Src ! resources_not_owned, resource_loop(Allocated, Waiting, Resources)
            end;
        {request, Src, ResourceKeys} ->
            case lists:substract(ResourceKeys, Resources) of
                {} -> 
                    case is_allocation_available(Allocated, ResourceKeys) of
                        ok -> 
                            Owned = allocate_resources(Resources, ResourceKeys, Src),
                            resource_loop(maps:merge(Allocated, Owned), Waiting, Resources);
                        _ -> 
                            NewWaiting = Waiting#{Src => ResourceKeys},
                            resource_loop(Allocated, NewWaiting, Resources)
                    end;
                Elems -> Src ! {not_a_resouce, Elems}, resource_loop(Allocated, Waiting, Resources)
            end
    end.


allocate_resources(Resources, ResourceSet, Src) ->
    AllocMap = maps:with(ResourceSet, Resources),
    Src ! {ok, AllocMap},
    maps:map(fun(_, _) -> Src end, AllocMap).

select_next_candidate(_, none) -> none;
select_next_candidate(Allocated, {Queuee, Requested, NextIter}) ->
    case maps:with(Requested, Allocated) of
        #{} -> Queuee; % No colliding allocation, we can hand it over to him now!
        _ -> select_next_candidate(Allocated, maps:next(NextIter))
    end;
select_next_candidate(Allocated, WaitingIter) ->
    select_next_candidate(Allocated, maps:next(WaitingIter)).

%is_allocation_available(_, [], []) -> ok;
%is_allocation_available(_, [], Busy) -> Busy;
%is_allocation_available(Allocated, [Key | KeySet], Busy) ->
%    case maps:is_key(Key, Allocated) of
%        true -> is_allocation_available(Allocated, KeySet, [Key | Busy]);
%        false -> is_allocation_available(Allocated, KeySet, Busy)
%    end.

%is_allocation_available(Resources, ResourceKeys) ->
%    is_allocation_available(Resources, ResourceKeys, []).

is_allocation_available(_, []) -> ok;
is_allocation_available(Allocated, [Key | KeySet]) ->
    case maps:is_key(Key, Allocated) of
        true -> busy;
        false -> is_allocation_available(Allocated, KeySet)
    end.

is_valid_release(_, [], _) -> ok;
is_valid_release(Allocated, [Key | ReleaseKeys], Src) ->
    case maps:find(Key, Allocated, {}) of
        {} -> invalid;
        Val when Val == Src -> is_valid_release(Allocated, ReleaseKeys, Src);
        _ -> invalid
    end.

request(Allocator, R) ->
    Allocator ! {request, self(), R},
    receive
        {ok, Resources} -> Resources;
        Other -> Other
    end.

release(Allocator, R) ->
    Allocator ! {release, self(), R}.