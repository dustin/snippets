%%
%% arch-tag: BB8FE92A-9817-11D8-AD77-000A957659CC
%%

-module(mrecv).
-export([start/0, getdict/0, getval/1]).
-export([init/0]).

start() ->
	spawn(?MODULE, init, []).

init() ->
	register(mrecv, self()),
	process_flag(trap_exit, true),
	Port = open_port({spawn, "./mrecv"}, [{packet, 1}]),
	loop(Port, dict:new()).

loop(Port, Dict) ->
	receive
		{Port, {data, Data}} ->
			% io:format("Got ~p\n", [Data]),
			Vals = string:tokens(Data, "\t"),
			Key  = lists:nth(2, Vals),
			Val  = list_to_float(lists:nth(3, Vals)),
			loop(Port, dict:update(Key, fun(_) -> Val end, Val, Dict));
		{lookup, From, SN} ->
			From ! dict:fetch(SN, Dict),
			loop(Port, Dict);
		{getdict, From} ->
			From ! Dict,
			loop(Port, Dict);
		Unhandled ->
			io:format("Unhandled message:  ~p\n", [Unhandled]),
			loop(Port, Dict)
	end.

getdict() ->
	mrecv ! {getdict, self()},
	receive
		Rv -> Rv
	end.

getval(SN) ->
	mrecv ! {lookup, self(), SN},
	receive
		Rv -> Rv
	end.
