%%
%% arch-tag: A439761A-9117-11D8-99CD-000A957659CC
%%

-module(coursemp).
-export([twoprocess/1, echo/0, nprocess/2, ringProcess/1, nprocessStar/2]).

echo() ->
	receive {From, N} ->
		io:format("Sending back ~p from ~p\n", [N, self()]),
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
	W = spawn_link(coursemp, echo, []),
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

processMaker(S, F, 1) ->
	spawn_link(coursemp, F, [S]);
processMaker(S, F, M) ->
	processMaker(spawn_link(coursemp, F, [S]), F, M-1).

% Bounce N messages around a ring of M processes
nprocess(N, M) ->
	Start = processMaker(self(), ringProcess, M),
	loop(N, Start),
	Start ! stop.

%
% star
%

listBuilder(F, 1, Rv) ->
	[F() | Rv];
listBuilder(F, N, Rv) ->
	P = F(),
	listBuilder(F, N-1, [P | Rv]).

foreach(F, [H|T]) ->
	F(H), foreach(F, T);
foreach(_, []) -> ok.

ntimes(_, 0) -> ok;
ntimes(F, N) ->
	F(), ntimes(F, N-1).

broadcast2Way(Msg, Procs) ->
	foreach(fun (P) ->
		P ! Msg,
		receive Rv -> io:format("bcast: ~p received ~p\n", [self(), Rv]) end
		end, Procs).

nprocessStar(N, M) ->
	Procs = listBuilder(fun () -> spawn_link(coursemp, echo, []) end, M, []),
	ntimes(fun () -> broadcast2Way({self(), ping}, Procs) end, N).
