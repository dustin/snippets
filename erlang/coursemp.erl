%%
%% arch-tag: A439761A-9117-11D8-99CD-000A957659CC
%%

-module(coursemp).
-export([twoprocess/1, echo/0, nprocess/2, ringProcess/1]).

echo() ->
	receive {From, N} ->
		io:format("Sending back ~p\n", [N]),
		From ! N
	end,
	echo().

loop(0, _) -> 0;
loop(N, With) ->
	With ! {self(), N},
	receive
		Rv -> io:format("Received ~p\n", [Rv])
	end,
	loop(N - 1, With).

% Bounce N messages between two proceses
twoprocess(N) ->
	W = spawn(coursemp, echo, []),
	link(W),
	loop(N, W).

%
% nm stuff
%

ringProcess(S) ->
	receive
		{From, N} ->
			io:format("Sending ~p from ~p to ~p\n", [N, self(), S]),
			S ! {self(), N},
			ringProcess(S);
		stop ->
			io:format("Shutting down ~p\n", [self()]),
			S ! stop,
			true
	end.

spawnLinked(M, F, A) ->
	P = spawn(M, F, A),
	link(P),
	P.

processMaker(S, F, 1) ->
	spawnLinked(coursemp, F, [S]);
processMaker(S, F, M) ->
	processMaker(spawnLinked(coursemp, F, [S]), F, M-1).

% Bounce N messages around a ring of M processes
nprocess(N, M) ->
	Start = processMaker(self(), ringProcess, M),
	loop(N, Start),
	Start ! stop.

