%%
%% arch-tag: E278E1FA-9115-11D8-BF08-000A957659CC
%%

-module(tempconv).
-export([convert/2]).

% Simple temperature conversion.
convert(Unit, Temp) ->
	case Unit of
		c -> {f, (Temp * 9 / 5) + 32};
		f -> {c, (Temp - 32) * 5 / 9}
	end.
