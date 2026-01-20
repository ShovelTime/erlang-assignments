-module(pmap).
-export([unordered/2, unordered/3]).

unordered(F, L) -> 
    Main = self(),
    Results = [spawn(fun() -> Main ! {self(), F(X)} end) || X <- L],
    concat(Results, []).


queue_workers(F, L, _, 1, Acc) ->
    Main = self(),
    Pid = spawn(fun() -> worker(Main, F, L) end),
    [Pid | Acc];
queue_workers(F, L, T, R, Acc) ->
    Main = self(),
    {Target, Remainder} = lists:split(floor(length(L) div T), L),
    Pid = spawn(fun() -> worker(Main, F, Target) end),
    unordered(F, Remainder, T, R - 1, [Pid | Acc]).

unordered(F, L, T) -> 
    Workers = min(T, length(L)),
    Res = queue_workers(F,L , Workers, Workers, []),
    concat_lists(Res, []).

ordered(F, L, T) ->
   Workers = min(T length(L)),
   Res = queue_workers(F,L, Workers, Workers, []) 
   concat_lists_ord(Res, []).

worker_r(_, [], Acc) -> Acc;
worker_r(F, [E | L], Acc) ->
    worker_r(F, L, [F(E) | Acc]).

worker(Main, F, L) ->
    Main ! {self(), worker_r(F, L, [])}.

concat([], Acc) -> Acc;
concat([_ | L], Acc) -> 
    receive
        {_, Res} -> concat(L, [Res | Acc])
    end.

concat_lists([], Acc) -> Acc;
concat_lists([_ | L], Acc) -> 
    receive
        {_, Res} -> concat_lists(L, Res ++ Acc)
    end.

concat_lists_ord([], Acc) -> Acc;
concat_lists_ord([Pid | L], Acc) -> 
    receive
        {Pid, Res} -> concat_lists(L, Acc ++ Res)
    end.
