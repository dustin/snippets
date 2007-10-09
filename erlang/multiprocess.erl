-module(multiprocess).
-export([loop/0, echo/0]).

% A = spawn(multiprocess, loop, []).
% link(A).

loop() ->
	receive
		{From, Msg} ->
			io:format("~p got ~s from ~s\n", [self(), Msg, From]),
			loop() 
	end.

echo() ->
	receive
		echo -> io:format("Echo\n")
	end.
