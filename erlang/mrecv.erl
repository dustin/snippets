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
	{ok, GAddr}=inet:getaddr("225.0.0.37", inet),
	{ok, LAddr}=inet:getaddr("0.0.0.0", inet),
	{ok, Port} = gen_udp:open(6789, [{add_membership,{GAddr,LAddr}}]),
	loop(Port, dict:new()).

loop(Port, Dict) ->
	receive
		{udp, Port, Raddr, Rport, S} ->
			% io:format("~p\n", [lists:sublist(S, (length(S)-1))]),
			Vals = string:tokens(S, "\t"),
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
