%%
%% arch-tag: A439761A-9117-11D8-99CD-000A957659CC
%%

-module(coursemp).
-export([twoprocess/1, echo/0, nprocess/2]).
-export([ringProcess/1, nprocessStar/2]).
-export([mcpMonitor/1, mcp/1]).

echo() ->
	receive
		{From, N} ->
			io:format("Sending back ~p from ~p\n", [N, self()]),
			From ! N,
			echo();
		stop ->
			io:format("Stopping ~p\n", [self()]),
			true;
		Unknown ->
			error_logger:error_msg("~p exiting due to unhandled message:  ~p\n",
				[self(), Unknown])
	end.

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
	loop(N, W),
	W ! stop.

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

ntimes(_, 0) -> ok;
ntimes(F, N) ->
	F(), ntimes(F, N-1).

% two-way broadcast (get a response)
broadcast2Way(Msg, Procs) ->
	io:format("Broadcasting ~p to ~p\n", [Msg, Procs]),
	lists:foreach(fun (P) ->
		P ! Msg,
		receive Rv -> io:format("bcast: ~p received ~p\n", [self(), Rv]) end
		end, Procs).

% Plain broadcast
broadcast(Msg, Procs) ->
	io:format("Broadcasting ~p to ~p\n", [Msg, Procs]),
	lists:foreach(fun (P) -> P ! Msg end, Procs).

nprocessStar(N, M) ->
	Procs = listBuilder(fun () -> spawn_link(coursemp, echo, []) end, M, []),
	ntimes(fun () -> broadcast2Way({self(), ping}, Procs) end, N),
	broadcast(stop, Procs).

%
% Master Control Process
%

mcpReplaceProcess(OldPid, Procs) ->
	NewProc = spawn_link(coursemp, echo, []),
	erlang:monitor(process, NewProc),
	io:format("Created new proc ~p\n", [NewProc]),
	OtherProcs = lists:filter(fun (P) -> not(P == OldPid) end, Procs),
	[NewProc | OtherProcs].

mcpLoop(Procs) ->
	io:format("Looping with procs:  ~p\n", [Procs]),
	receive
		{sendto, N, Msg} ->
			lists:nth(N, Procs) ! Msg,
			mcpLoop(Procs);
		{'DOWN', _Ref, process, Pid, _Flag} ->
			io:format("MCP got down message for ~p\n", [Pid]),
			mcpLoop(mcpReplaceProcess(Pid, Procs));
		Msg ->
			io:format("MCP got ~p\n", [Msg]),
			mcpLoop(Procs)
	end.

mcpMonitor(Procs) ->
	lists:foreach(fun (P) -> erlang:monitor(process, P) end, Procs),
	mcpLoop(Procs).

mcp(N) ->
	Procs = listBuilder(fun () -> spawn_link(coursemp, echo, []) end, N, []),
	spawn_link(coursemp, mcpMonitor, [Procs]).
