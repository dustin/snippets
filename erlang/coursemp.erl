%%
%% arch-tag: A439761A-9117-11D8-99CD-000A957659CC
%%

-module(coursemp).
-export([twoprocess/1, echo/0]).

echo() ->
	receive {From, N} ->
		io:format("Sending back ~p\n", [N]),
		From ! N
	end,
	echo().

loop(0, _) -> 0;
loop(N, With) ->
	io:format("Sending ~p\n", [N]),
	With ! {self(), N},
	io:format("Receiving\n"),
	receive
		Rv -> io:format("Received ~p\n", [Rv])
	end,
	loop(N - 1, With).

twoprocess(N) ->
	W = spawn(coursemp, echo, []),
	link(W),
	loop(N, W).
