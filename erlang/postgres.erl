%%
%% arch-tag: 9081D292-9A71-11D8-B144-000A957659CC
%%

%%
%% Postgres client
%%

-module(postgres).
-behavior(gen_fsm).

% Client functions
-export([start/4]).

% States
-export([authenticated/2, ready_for_query/2]).

% callbacks
-export([handle_event/3, handle_sync_event/4,
	handle_info/3, init/1, terminate/3]).

-record(conninfo, {host,port,user,pass,db}).
-record(cancelinfo, {cpid,ckey}).
-record(pginfo, {conninfo,cancelinfo,socket,sstat}).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Client functions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Host, Port, User, Database) ->
	gen_fsm:start(?MODULE, [Host,Port,User,Database],[]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Make a string with its length in postgres FE/BE stream format.
makeString(String) ->
	StrLength = length(String)+4,
	[binary_to_list(<<StrLength:32/integer-big>>)|String].

assert(V, Msg) ->
	case V of
		true ->
			true;
		_ ->
			exit(Msg)
	end.

% Get the first four items from a list and create a 32-bit int out of them.
% Return the 32-bit number and the remaining list.
getInt32(Data) when length(Data) >= 4 ->
	% Extract the part we care about from the rest
	{FirstFour, Rest} = lists:split(4, Data),
	% Create our int (big-endian).
	Rv = lists:foldl(fun(A, In) when (A >= 0) and (256 > A) ->
			In * 256 + A end,
		0, FirstFour),
	% Return the number and the list
	{Rv, Rest}.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callbacks
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Establish the TCP connection and let the other end know who we are
init([Host,Port,User,Database]) ->
	io:format("Connecting to ~p:~p~n", [Host,Port]),
	% Connect
	{ok, Socket} = gen_tcp:connect(Host, Port, [list,{packet,0}]),
	io:format("Socket is ~p~n", [Socket]),
	% Announce
	Announcement = lists:append([binary_to_list(<<0,3,0,0>>), % [196608],
		"user", binary_to_list(<<0>>), User, binary_to_list(<<0>>),
		"database", binary_to_list(<<0>>), Database, binary_to_list(<<0,0>>)]),
	ok = gen_tcp:send(Socket, makeString(Announcement)),
	Conninfo=#conninfo{host=Host, port=Port, user=User,
	        pass="blahblah", db=Database},
	{ok, connected, #pginfo{conninfo=Conninfo, socket=Socket,
		sstat=dict:new()}}.

% Data arriving after authentication

authenticated({data_arrived, Type, Extra}, Info) ->
	% io:format("Data arrived in authenticated state, type ~c~n", [Type]),
	handle_packet([Type|Extra], authenticated, Info).

ready_for_query({data_arrived, Type, Extra}, Info) ->
	io:format("Data arrived in ready_for_query state, type ~c~n", [Type]),
	handle_packet([Type|Extra], authenticated, Info).

%%%%% authentication handlers

% Perform MD5 authentication
perform_auth(auth_md5, Data, connected, Info) ->
	Salt = lists:nthtail(4, Data),
	Conninfo = Info#pginfo.conninfo,
	UplusP = conversions:md5_hex(Conninfo#conninfo.pass
		++ Conninfo#conninfo.user),
	% io:format("hexed password/user (phase 1):  ~p~n", [UplusP]),
	Digested = conversions:md5_hex(UplusP ++ Salt),
	% io:format("digested password (phase 2):  ~p~n", [Digested]),
	ok = gen_tcp:send(Info#pginfo.socket,
		[$p|makeString("md5" ++ Digested ++ "\0")]),
	{next_state, connected, Info};
% Auth OK message, change to authenticated state
perform_auth(auth_ok, Data, connected, Info) ->
	{next_state, authenticated, Info};
% No other auth type is allowed
perform_auth(Type, Data, connected, Info) ->
	exit("Trying to use unhandled auth type " ++ atom_to_list(Type)).

%%%%% end of authentication handlers

% Auth request
handle_packet($R, Length, Data, connected, Info) ->
	{TypeNum, Rest} = getInt32(Data),
	Type = case TypeNum of
		0 -> auth_ok;
		1 -> auth_k4;
		2 -> auth_k5;
		3 -> auth_clear;
		4 -> auth_crypt;
		5 -> auth_md5;
		6 -> auth_scm
	end,
	io:format("Got auth message (~p):  ~p bytes:  ~p~n", [Type, Length, Rest]),
	perform_auth(Type, Data, connected, Info);

% Errors
handle_packet($E, Length, Data, State, Info) ->
	error_logger:error_msg("Got an error in state ``~p'':  ~p~n",
		[State, lists:flatten(
			lists:map(fun(P) -> case P of 0 -> " - "; _ -> P end end, Data))]),
	{next_state, State, Info};

% Cancellation key
handle_packet($K, Length, Data, State, Info) ->
	{Pid, Key} = getInt32(Data),
	io:format("Got cancellation key (~p) for pid (~p)~n", [Key, Pid]),
	Cancelinfo = #cancelinfo{ckey = Key, cpid = Pid},
	{next_state, State, Info#pginfo{cancelinfo = Cancelinfo}};

% Ready for query
handle_packet($Z, Length, Data, State, Info) ->
	io:format("Ready for query~n", []),
	io:format("State of the union:  ~p~n", [Info]),
	{next_state, ready_for_query, Info};

% parameter status
handle_packet($S, Length, Data, State, Info) ->
	[Key,Val] = string:tokens(Data, "\0"),
	io:format(" - ~p = ~p~n", [Key, Val]),
	Sstat = Info#pginfo.sstat,
	MSstat = dict:update(Key, fun(_) -> Val end, Val, Sstat),
	{next_state, State, Info#pginfo{sstat = MSstat}};

% Any other packet
handle_packet(Type, Length, Data, State, Info) ->
	io:format("Got unhandled packet ~c length ~p [~p] in state ~p~n",
		[Type, Length, Data, State]),
	{next_state, State, Info}.

dealWithRemaining([Type|Data], State, Info) ->
	% io:format("Remaining data:  ~c (~p bytes)~n", [Type, length(Data)]),
	gen_fsm:send_event(self(), {data_arrived, Type, Data});
dealWithRemaining([], _State, _Info) ->
	[].

handle_packet([Type|Data], State, Info) ->
	{Len, Rest} = getInt32(Data),
	% io:format("Got packet of type ~c, ~p bytes~n", [Type,Len]),
	{FullData, Extra} = getAllData(Len, Rest, Info),
	assert(length(FullData)+4 == Len, "Incorrect data length"),
	dealWithRemaining(Extra, State, Info),
	handle_packet(Type, Len, FullData, State, Info).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callbacks
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

getAllData(Len, Rest, Info) ->
	DLen=length(Rest) + 4,
	Remaining = Len - DLen,
	if (Remaining < 1) ->
			if (Remaining == 0) ->
					{Rest, []}; % Nothing left, nothing over
				true ->
					lists:split(Len-4, Rest)
			end;
		true ->
			io:format("Need to read ~p more bytes~n", [Remaining]),
			{ok, Pkt} = gen_tcp:recv(Info#pginfo.socket, Remaining),
			io:format("Read ~p~n", [Pkt]),
			getAllData(Len, Rest ++ Pkt, Info)
	end.

% Auth request
handle_info({tcp, _Port, Data}, State, Info) ->
	handle_packet(Data, State, Info).

% XXX  Doesn't do anything useful yet
handle_sync_event(X, Pid, AnyState, Info) ->
	io:format("Handling sync event (~p, ~p, ~p, ~p)~n",
		[X, Pid, AnyState, Info]),
	{ok, connected, Info}.

% XXX  Doesn't do anything useful yet
handle_event(X, AnyState, Info) ->
	io:format("Handling event (~p, ~p, ~p)~n", [X, AnyState, Info]),
	{ok, connected, Info}.

% XXX  Doesn't do anything useful yet
terminate(Reason,StateName,StateData) ->
	{terminated, Reason}.
