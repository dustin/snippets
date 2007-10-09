-module(sup_test).
-behavior(supervisor).

% supevisor stuff
-export([start_link/0, init/1]).

% child stuff
-export([new_child/0]).

%
% Supervisor stuff
%

start_link() ->
	supervisor:start_link(sup_test, []).

init(_Args) ->
	{ok, {{one_for_one, 2, 60},
			[{sup_test_child, {sup_test, new_child, []},
				permanent, 5000, worker, [sup_test]}]}}.

%
% Child stuff
%

new_child() ->
	io:format("Spawning a new child...\n"),
	{ok, spawn_link(coursemp, echo, [])}.

