-module(task1).
-export([eval/1, eval/2, map/2, groupby/2, split/2, filter/2]).



eval({Operator, X, {OperatorT, XT, YT}}) ->
	case eval({OperatorT, XT, YT}) of 
			 {ok, Value} -> case eval({Operator, X, Value}) of
						error -> error;
						Val -> Val      
				       end
			 
	end;
eval({Operator, {OperatorS, XS, YS}, Y}) ->
	case eval({OperatorS, XS, YS}) of 
			 {ok, Value} -> case eval({Operator, Value, Y}) of
					       error -> error;
					       Val -> Val
				       end
	end;
eval({add, X, Y}) ->
	{ok, X + Y};
eval({sub, X, Y}) ->
	{ok, X - Y};
eval({mul, X, Y}) -> 
	{ok, X * Y};
eval({'div', X, Y}) ->
	{ok, X / Y};

eval({Operator, X, Y}) when is_function(Operator) -> 
	try Operator(X,Y) of
		error -> error;
		N -> {ok, N}
	catch 
		error:_ -> error 
	end. 
eval({Operator, X, Y}, Map) when is_atom(X) or is_atom(Y) ->
	try 
		ResX = maps:get(X, Map),
		ResY = maps:get(Y, Map),
		eval({Operator, ResX, ResY}, Map)
	of
		{ok, Val} -> {ok, Val}
	catch
		error:{badmap, _} -> {error, unknown_error};
		error:{badkey, K} when is_atom(K) -> {error, unknown_variable};
		error:{badkey, K} when K == X -> 
			try maps:get(Y, Map) of
				VarY -> eval({Operator, X, VarY}, Map)
			catch
				error:{badkey, _} -> {error, unknown_variable}
			end;
		error:{badkey, K} -> eval({Operator, maps:get(K, Map), Y}, Map)
	end;
%eval({Operator, X, Y}, Map) when is_atom(X) or is_atom(Y) ->
%	maybe 
%		{ok, ResX} ?= maps:get(X, Map),
%		{ok, ResY} ?= maps:get(Y, Map),
%		eval({Operator, ResX, ResY})
%	else
%		{badmap, _} -> {error, unknown_error};
%		{badkey, K} when is_atom(K) -> {error, unknown_variable};
%		{badkey, K} when K == X -> 
%			try maps:get(Y, Map) of
%				{ok, VarY} -> eval({Operator, X, VarY}, Map)
%			catch
%				{badkey, _} -> {error, unknown_variable}
%			end;
%		{badkey, K} when K == Y -> eval({Operator, maps:get(X), Y}, Map)
%	end;
eval({Operator, {OperatorX, XX, YX} , Y}, Map) -> 
	try eval({OperatorX, XX, YX}, Map) of
		{ok, X} -> eval({Operator, X, Y}, Map)
	catch
		error:{error, Err} -> {error, Err}
	end;

eval({Operator, X ,{OperatorY, XY, YY}}, Map) ->
	try eval({OperatorY, XY, YY}, Map) of
		{ok, Y} -> eval({Operator, X, Y}, Map)
	catch
		error:{error, Err} -> {error, Err}
	end;
eval({Operator, X, Y}, _) ->
	eval({Operator, X, Y}).

map(_, []) -> [];
map(F, [H|R])-> [F(H) | map(F, R)];
map(F, H) -> F(H).

split(_, {T,F}, []) -> {lists:reverse(T),lists:reverse(F)};
split(P, {T,F}, [E|L]) ->
	case P(E) of
		true -> split(P, {[E|T], F}, L);
		false -> split(P, {T, [E|F]}, L)
	end.
split(_, []) -> {[], []};
split(P, L) -> split(P, {[],[]}, L).

groupby(_, M, [], _) -> maps:map(fun(_, K) when is_list(K) -> lists:reverse(K) end, M);
groupby(F, M, [E|L], I) -> 
	try maps:get(F(E), M, I) of
	    I -> groupby(F, M#{F(E) => [I]}, L, I + 1);
		Value -> groupby(F, M#{F(E) => [I|Value]},L, I + 1)
	catch
		error:{badmap, _} -> {error, unknown_error}
	end.
groupby(_, []) -> #{};
groupby(F, L) -> groupby(F, #{}, L, 1). 

filter(_, []) -> [];
filter(P, L) -> filter(P, [], L).
filter(_, Acc, []) -> lists:reverse(Acc);
filter(P, Acc, [E|L]) -> 
	case P(E) of
		true -> filter(P, [E|Acc], L);
		_ -> filter(P, Acc, L)
	end.

