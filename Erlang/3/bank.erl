-module(bank).
-behavior(gen_server).
-export([start/0, init/1, handle_call/3, handle_cast/2, lend/4, withdraw/3, deposit/3, balance/2]).

start() -> {ok, Pid} = gen_server:start(?MODULE, [], []), Pid.


init(_) ->
	{ok, #{}}.

handle_call(Req, _, State) ->
	handle_req(Req, State).

			
handle_cast(Req, State) ->
	{_, _, NewState} = handle_req(Req, State),
	{noreply, NewState}.

handle_req(Req, State) -> 
	case Req of 
		{balance, Who} ->
			try maps:get(Who, State) of 
                        	Val -> {reply, {ok, Val}, State}
                    	catch 
                        	error:_ -> {reply, no_account, State}
			end;
		{deposit, {Who, Amount}} ->
			try maps:get(Who, State) of
                        	Val -> NewAcc = State#{Who => Amount + Val}, {reply, {ok, Val + Amount}, NewAcc}
                    	catch
                        	error:_ -> NewAcc = State#{Who => Amount}, {reply, {ok, Amount}, NewAcc}
                	end;
		{withdraw, {Who, Amount}} ->
                    	try maps:get(Who, State) of
                        	Val -> 
                            		case Val - Amount of
                                		Negative when Negative < 0 -> {reply, insufficient_funds, State};
                                		NewVal -> NewAcc = State#{Who => NewVal}, {reply, {ok, NewVal}, NewAcc}
                            		end
        	  	catch
                        	error:_ -> {reply, no_account, State}
                	end;
		{lend, {From, To, Amount}} ->
                	try maps:get(From, State) of 
                        	FromVal ->
                            		try maps:get(To, State) of 
                                		ToVal -> 
                                    			case FromVal - Amount of
                                        			Negative when Negative < 0 -> {reply, insufficient_funds, State};
                                        			NewVal -> NewAcc = State#{From => NewVal, To => ToVal + Amount}, {reply, ok, NewAcc}
                                    			end 
                            		catch
                                		error:_ -> {reply, {no_account, To}, State}
                            		end
			catch
                        	error:_ ->
					try maps:get(To, State) of 
						_ -> {reply, {no_account, From}, State}
					catch 
                                		error:_ -> {reply, {no_account, both}, State} 
                            		end 
			end
	end.	

balance(Pid, Who) ->
    Ref = make_ref(),
    call(Pid, Ref, {balance, Who}).

deposit(Pid, Who, X) ->
    Ref = make_ref(),
    call(Pid, Ref, {deposit, {Who, X}}).

withdraw(Pid, Who, X) ->
    Ref = make_ref(),
    call(Pid, Ref, {withdraw, {Who, X}}).

lend(Pid, From, To, X) ->
    Ref = make_ref(),
    call(Pid, Ref, {lend, {From, To, X}}).




call(Pid, _, Msg) ->
	case is_process_alive(Pid) of 
	   false -> no_bank;
	   true ->
		ReqId = gen_server:send_request(Pid, Msg),
		case gen_server:receive_response(ReqId, 500) of
			timeout -> no_bank;
			{_, Response} -> Response
		end
	end.


