-module(double).
-export([start/0]).

start() ->
	PROCID = spawn(double, doubler, []),
	register(double, PROCID),
	PROCID.



doubler() ->

	receive 

		{Pid, _, N} -> Pid ! N*2
	end.
