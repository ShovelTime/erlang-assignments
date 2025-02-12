-module(monitor).
-export([start/0, init/1]).

start() ->
%    spawn(fun() -> monitor_loop() end).
	supervisor:start_link(monitor, {}).
	


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

init(_) ->
	{ok, {#{strategy => simple_one_for_one}, [#{ 
			id => double,
			start => {double, start, {}}
		}]
	     }
	}. 
