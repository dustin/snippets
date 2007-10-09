-module(test_proc).
-export([start_link/0, init/0]).

start_link() ->
	{ok, spawn_link(?MODULE, init, [])}.

init() ->
	error_logger:info_msg("Running test proc~n", []),
	register(?MODULE, self()),
	loop().

loop() ->
	receive Rv -> error_logger:error_msg("Got message ~p~n", [Rv]) end,
	loop().
