%%
%% arch-tag: 8026944F-931F-11D8-9FE9-000393CFE6B8
%%

-module(d_utils).
-export([ps/1, ps/0, show_process/1]).

% Show a process the way I want to see it
show_process(P) ->
	io:format("~p\t~p\t~p~n",
		[P,
			element(2,process_info(P, status)),
			element(2,process_info(P, initial_call))]).

% Show processes using the provided display function
ps(DisplayFunction) ->
	lists:foreach(DisplayFunction, processes()).

% Show proceses using the default display function
ps() ->
	ps(fun show_process/1).
