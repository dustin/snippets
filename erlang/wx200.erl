%%
%% arch-tag: E2F5BB8F-A557-11D8-B164-000A957659CC
%%

-module(wx200).

% API
-export([start/2, start_link/2, init/2]).

% Readings
-record(temp_reading, {indoor, outdoor}).
-record(rain_reading, {val}).
-record(bar_reading, {trend, pred, pressure, in_dew, out_dew}).
-record(hum_reading, {indoor,outdoor}).
-record(wind_reading, {val}).
-record(time_reading, {hour, minute, second}).

start(Host, Port) ->
	spawn(?MODULE, init, [Host, Port]).

start_link(Host, Port) ->
	spawn_link(?MODULE, init, [Host, Port]).

% Initialize the socket
init(Host, Port) ->
	{ok, S} = gen_tcp:connect(Host, Port, [binary,{active,false}]),
	mainLoop(S).

% Packet identification
packetType(16#8f) -> {time_humidity, 34};
packetType(16#9f) -> {temp, 33};
packetType(16#af) -> {barometer_dew, 30};
packetType(16#bf) -> {rain, 13};
packetType(16#cf) -> {wind, 26};
packetType(16#ff) -> {time, 4};
packetType(X) ->
	exit(io_lib:format("Invalid first byte in packet:  ~.16B", [X])).

% Packet decoding
decodePacket({time, Bytes}) ->
	<<I0, I1, I2, _/binary>> = Bytes,
	#time_reading{hour=bcd(I2), minute=bcd(I1), second=bcd(I0)};
decodePacket({temp, Bytes}) ->
	<<I0, I1, _:13/binary, I15, I16, _/binary>> = Bytes,
	#temp_reading{indoor=decodeTemp(I0, I1), outdoor=decodeTemp(I15, I16)};
decodePacket({rain, Bytes}) ->
	<<I0, I1, _/binary>> = Bytes,
	#rain_reading{val=bcd(I0) + (100 * (bcd(16#0f band I1)))};
decodePacket({time_humidity, Bytes}) ->
	#hum_reading{indoor=bcd(getByte(7, Bytes)),
				outdoor=bcd(getByte(19, Bytes))};
decodePacket({wind, Bytes}) ->
	<<I0, I1, _/binary>> = Bytes,
	#wind_reading{val=bcd(I0) + (100 * (bcd(16#0f band I1)))};
decodePacket({barometer_dew, Bytes}) ->
	Trend = case((16#70 band getByte(5, Bytes)) bsr 4) of
		1 -> falling;
		2 -> steady;
		4 -> rising;
		X1 -> erlang:fault(io_lib:format("invalid trend:  ~.16B", [X1]))
	end,
	Pred = case(16#0f band getByte(5, Bytes)) of
		1 -> sunny;
		2 -> cloudy;
		4 -> partly_cloudy;
		8 -> rainy;
		X2 -> erlang:fault(io_lib:format("invalid prediction:  ~.16B", [X2]))
	end,
	Pressure = (bcd(getByte(1, Bytes)) * 100) + bcd(getByte(0, Bytes)),
	InDew = bcd(getByte(6, Bytes)),
	OutDew = bcd(getByte(17, Bytes)),
	#bar_reading{trend=Trend, pred=Pred, pressure=Pressure,
				in_dew=InDew, out_dew=OutDew};
decodePacket({Type, Bytes}) ->
	io:format("Ignoring packet of type ~p~n", [Type]).

% The loop
mainLoop(S) ->
	{ok, <<Byte>>} = gen_tcp:recv(S, 1),
	{Type, Bytes} = packetType(Byte),
	{ok, Data} = gen_tcp:recv(S, Bytes),
	Decoded = decodePacket({Type, Data}),
	io:format("Decoded:  ~p~n", [Decoded]),
	mainLoop(S).

% Get byte N from the binary thing
getByte(N, Bytes) ->
	<<_:N/binary, B, _/binary>> = Bytes, B.

% BCD decoder
bcd(X) ->
	(((X band 16#f0) bsr 4) * 10) + (X band 16#0f).

% Temperature decoder
decodeTemp(I1, I2) ->
	Bc = float(bcd(I1)),
	A = float(I2 band 16#7),
	D = float(A * 10.0) + (Bc / 10.0),
	case (I2 band 16#8) of
		0 -> D;
		_ -> 0 - D
	end.
