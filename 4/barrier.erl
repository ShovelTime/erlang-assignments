-module(barrier).
-export([start/1, wait/2]).

start(Blockers) when is_list(Blockers) ->
    spawn(fun() -> blocker_loop([], ordset:new(), Blockers) end).

blocker_loop(Waiting, BlockingRefs, []) ->
    wake_up_blocked(Waiting),
    blocker_loop([], ordset:new(), ordset:to_list(BlockingRefs));
blocker_loop(Waiting, BlockingRefs, Blockers) ->
    receive
        {waiting, Src, Ref} -> 
            case lists:member(Ref, Blockers) of
                true -> blocker_loop([Src | Waiting], ordset:add_element(Ref, BlockingRefs), lists:delete(Ref, Blockers));
                false -> 
                    Src ! continue,
                    blocker_loop(Waiting, BlockingRefs, Blockers)
            end
    end.

wake_up_blocked([]) -> [];
wake_up_blocked([Pid | Blocked]) ->
    Pid ! continue,
    wake_up_blocked(Blocked).


wait(Barrier, Ref) ->
    Barrier ! {waiting, self(), Ref},
    receive 
        continue -> continue
    end.