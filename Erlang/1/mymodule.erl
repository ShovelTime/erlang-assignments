-module(mymodule).
-export([foo/1, bar/2]).
-import(lists, [map/2, seq/2]).
-record(foobar, {foo=foo, bar}).
-myattr(an_atom).
-compile(export_all). %% add

foo(_) -> true.
foo() -> false.
bar(X, _) -> map(fun foo/1, seq(0, 10)).

test_match(X) ->
    case X of
	{ok, H} ->
	    H;
	[_A,B|_] ->
	    B;
	[{1,A}|_] ->
	    A;
	[1,2|T] -> %% Not matched
	    T;
	{A,A} ->
	    same;
	{_,_} ->
	    different
    end.

test_if(X) ->
    if is_function(X) ->
	    function;
       is_number(X) ->
	    number;
       length(X) =< 2 ->
	    short_list;
       length(X) > 2 ->
	    long_list;
       %% foo(X) -> %% ADD LATER
       %% 	    foo_called;
       true ->
	    no_match
    end.
