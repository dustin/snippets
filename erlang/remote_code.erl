%%
%% arch-tag: 0AEAC98A-258B-11D9-8975-000A957659CC
%%
%% My attempt at a remote code server...I'm not certain it's any better than
%% just using code, but I'm kind of hoping for something.
%%

-module(remote_code).
-export([start/0, start_link/0, init/0]).
-export([load_file/2]).

start() ->
	spawn(?MODULE, init, []).

start_link() ->
	spawn_link(?MODULE, init, []).

% Exported functions
load_file(Node, Mod) ->
	error_logger:info_msg("Sending load_file request of ~p from ~p",
		[Mod, self()]),
	?MODULE ! {load_file, Mod, self()}.

% Initialize the remote code server
init() ->
	register(?MODULE, self()),
	mainLoop().

mainLoop() ->
	error_logger:error_msg("Looping new code"),
	receive
		{ping, Pid} ->
			Pid ! pong,
			mainLoop();
		{load_file, Mod, Remote} ->
			error_logger:error_msg("Soft purging:  ~p", [Mod]),
			code:soft_purge(Mod),
			Remote ! code:load_file(Mod),
			mainLoop();
		Unknown ->
			error_logger:error_msg("Got unknown message:  ~p", [Unknown]),
			mainLoop()
	end.
