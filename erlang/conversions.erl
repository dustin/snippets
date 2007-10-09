%%
%% Data converisons
%%

-module(conversions).
-export([hex/1, md5_hex/1]).

% Convert something to a hex string
hex(B) when binary(B) ->
	hex(binary_to_list(B));
hex(L) when list(L) ->
	lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
	[hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I) ->
	[$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I) ->  $0 +I.

% Get an md5 as a hex string.
md5_hex(Data) ->
	hex(erlang:md5(Data)).
