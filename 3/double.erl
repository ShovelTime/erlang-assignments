-module(double).
-export([start/0, double/1]).

start() ->
	PROCID = spawn(fun() -> double() end),
	register(double, PROCID),
	{ok,PROCID}.


double() ->
	receive 
		{Pid, Ref, N} -> Pid ! {Ref, N*2}
	end,
    double().

double(N) ->
    N * 2.
