-module(ring).
-export([start/2]).


start(N, M) -> 
	First = init_ring(N, self()),
	loop_val(M, 0, First).


loop_val(0, Val, _) -> Val;
loop_val(Remaining, Val, Start) ->
	Start ! Val,
	receive 
		NewVal -> loop_val(Remaining - 1, NewVal, Start)
	end.

init_ring(0, First) -> First;
init_ring(Remaining, Next) ->
	Pid = spawn(fun() -> increment(Next) end),
	init_ring(Remaining - 1, Pid).


increment(To) ->
	receive 
		N -> To ! N + 1
	end,
	increment(To).
