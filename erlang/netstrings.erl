%%
%% arch-tag: 9840C006-9D87-11D8-83CB-000A957659CC
%%

-module(netstrings).
-export([encode/1]).

encode(S) ->
	integer_to_list(length(S)) ++ ":" ++ S ++ ",".
