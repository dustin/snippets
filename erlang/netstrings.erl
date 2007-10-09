-module(netstrings).
-export([encode/1]).

encode(S) ->
	integer_to_list(length(S)) ++ ":" ++ S ++ ",".
