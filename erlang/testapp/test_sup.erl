%%
%% arch-tag: 8DD7AF50-A062-11D8-9B0A-000393CFE6B8
%%

-module(test_sup).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link(?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 2, 60},
		[{test_proc, {test_proc, start_link, []},
			permanent, 5000, worker, [test_proc]}]}}.
