-module(bank).
-export([start/0, lend/4, withdraw/3, deposit/3, balance/2]).

start() -> spawn(fun() -> bank_loop(#{}) end).
    


bank_loop(Accounts) ->
    receive
        {Src, Ref, Cmd, Args} ->
            case Cmd of
                balance ->
                    try maps:get(Args, Accounts) of 
                        Val -> Src ! {Ref, {ok, Val}}, bank_loop(Accounts)
                    catch 
                        error:_ -> Src ! {Ref, no_account}, bank_loop(Accounts)
                    end;
                deposit ->
                    {Who, Amount} = Args,
                    try maps:get(Who, Accounts) of
                        Val -> NewAcc = Accounts#{Who => Amount + Val}, Src ! {Ref, {ok, Val + Amount}}, bank_loop(NewAcc)
                    catch
                        error:_ -> NewAcc = Accounts#{Who => Amount}, Src ! {Ref, {ok, Amount}}, bank_loop(NewAcc)
                    end;
                withdraw ->
                    {Who, Amount} = Args,
                    try maps:get(Who, Accounts) of
                        Val -> 
                            case Val - Amount of
                                Negative when Negative < 0 -> Src ! {Ref, insufficient_funds}, bank_loop(Accounts);
                                NewVal -> NewAcc = Accounts#{Who => NewVal}, Src ! {Ref, {ok, NewVal}}, bank_loop(NewAcc)
                            end
                    catch
                        error:_ -> Src ! {Ref, no_account}, bank_loop(Accounts)
                    end;
                lend ->
                    {From, To, Amount} = Args,
                    try maps:get(From, Accounts) of 
                        FromVal ->
                            try maps:get(To, Accounts) of 
                                ToVal -> 
                                    case FromVal - Amount of
                                        Negative when Negative < 0 -> Src ! {Ref, insufficient_funds}, bank_loop(Accounts);
                                        NewVal -> NewAcc = Accounts#{From => NewVal, To => ToVal + Amount}, Src ! {Ref, ok}, bank_loop(NewAcc)
                                    end 
                            catch
                                error:_ -> Src ! {Ref, {no_account, To}}, bank_loop(Accounts)
                            end
                    catch
                        error:_ ->
                            try maps:get(To, Accounts) of 
                                _ -> Src ! {Ref, {no_account, From}}, bank_loop(Accounts)
                            catch 
                                error:_ -> Src ! {Ref, {no_account, both}}, bank_loop(Accounts)
                            end 
                    end
            end
    end.


balance(Pid, Who) ->
    Ref = make_ref(),
    call(Pid, Ref, {self(), Ref, balance, Who}).

deposit(Pid, Who, X) ->
    Ref = make_ref(),
    call(Pid, Ref, {self(), Ref, deposit, {Who, X}}).

withdraw(Pid, Who, X) ->
    Ref = make_ref(),
    call(Pid, Ref, {self(), Ref, withdraw, {Who, X}}).

lend(Pid, From, To, X) ->
    Ref = make_ref(),
    call(Pid, Ref, {self(), Ref, lend, {From, To, X}}).

call(Pid, Ref, Msg) ->
    process_flag(trap_exit, true),
    MonitorRef = monitor(process, Pid),
    Pid ! Msg,
    receive 
        {'DOWN', MonitorRef, process, Pid, _} -> 
            demonitor(MonitorRef),
            no_bank;
        {Ref, Res} ->
            demonitor(MonitorRef),
            Res
    after 500 -> 
        no_bank
    end.
