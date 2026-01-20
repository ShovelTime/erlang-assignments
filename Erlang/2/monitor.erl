-module(monitor).
-export([start/0]).

start() ->
    spawn(fun() -> monitor_loop() end).

monitor_loop() ->
    process_flag(trap_exit, true),
    Pid  = double:start(),
    link(Pid),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} ->
            demonitor(Ref),
            monitor_loop()    
    end.
