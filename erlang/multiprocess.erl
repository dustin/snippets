%%
%% arch-tag: FF49E62C-9047-11D8-ACD0-000A957659CC
%%

-module(multiprocess).
-export([loop/0]).

loop() ->
	receive
		something ->
			io:format("I am ~p~n", [self()]),
			loop()
	end.
