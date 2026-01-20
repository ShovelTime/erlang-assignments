-module(gen_produceconsume).
-export([start/2, produce/2, consume/1, stop/1]).
-callback handle_produce(T :: term()) -> {Res :: term(), Task :: term()}.
-callback handle_consume(T :: term()) -> Res :: term().


start(Module, BufferSize) ->
	spawn(fun() -> main_loop(Module, [], BufferSize) end).

stop(Pid) ->
	exit(Pid, kill).

main_loop(Module, Buffer, MaxSize) when length(Buffer) == 0 ->
	receive 
		{Pid, T} -> 
			Res = Module:handle_produce(T),
			Pid ! ok,
			main_loop(Module, lists:append(Buffer, [Res]), MaxSize)
		end;

main_loop(Module, [T | Buffer], MaxSize) when length(Buffer) >= MaxSize ->
	receive 
		Pid -> 
			Res = Module:handle_consume(T),
			Pid ! Res,
			main_loop(Module, Buffer, MaxSize)
	end;
main_loop(Module, Buffer, MaxSize) ->
	receive 
		{Pid, T} -> 
			Res = Module:handle_produce(T),
			Pid ! ok,
			main_loop(Module, lists:append(Buffer, [Res]), MaxSize);
		Pid ->
			[T | NewBuf] = Buffer,
			Res = Module:handle_consume(T),
			Pid ! Res,
			main_loop(Module, NewBuf, MaxSize)
	end.




produce(Pid, T) ->
	Pid ! {self(), T},
	receive
		Response -> Response
	end.

consume(Pid) -> 
	Pid ! self(), 
	receive 
		Response -> Response
	end.
