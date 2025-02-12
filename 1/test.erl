-module(test).
-compile(export_all).

apply_to(_, []) ->
    [];
apply_to(F, [H|T]) ->
    [F(H) | apply_to(F, T)].

test() ->
    Sum = fun (_, []) ->
		  0;
	      (Me, [H|T])->
		  H + Me(Me, T)
	  end,
    Sum(Sum, [1,2,3]).

extensions() ->
    [filename:extension(F) || 
	F <- begin 
		 {ok, D} = file:list_dir("."), 
		 D 
	     end, 
	filelib:is_file(F)].

select(X, L) ->
    [Y || {X, Y} <- L, X =:= X].

head([H|_]) ->
    H.
tail([_H|T]) ->
    T.
second([_F, S|_]) ->
    S.

t({_, _}) ->
    false;
t({a,b}) ->
    true;
t({b, a}) ->
    true.

same_sign(X, Y) when X > 0, Y > 0;
		     X < 0, Y < 0 ->
    yes;
same_sign(X, Y) when is_number(X) andalso 
		     is_number(Y)->
    no.
	   

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

mul([]) -> 0;
mul([H|T]) -> H * mul(T).

map_plus_1([]) -> #{};
map_plus_1([H|T]) -> 
    V = map_plus_1(T),
    V#{H => H +1}.

accumulate(_, Result, []) -> Result;
accumulate(F, Result, [H|T]) -> F(H, accumulate(F, Result, T)).

%% sum(L) -> accumulate(fun (V, Acc) -> V + Acc end, 0, L).
%% mul(L) -> accumulate(fun (V, Acc) -> V * Acc end, 0, L).
%% map_plus_1(L) -> accumulate(fun (V, Acc) -> Acc#{V => V + 1} end, #{}, L).

	      

