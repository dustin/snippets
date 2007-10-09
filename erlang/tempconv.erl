-module(tempconv).
-export([convert/2]).

% Simple temperature conversion.
convert(Unit, Temp) ->
	case Unit of
		c -> {f, (Temp * 9 / 5) + 32};
		f -> {c, (Temp - 32) * 5 / 9}
	end.
