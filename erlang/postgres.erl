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
-export([connected/3]).

% callbacks
-export([handle_event/3, handle_sync_event/4,
	handle_info/3, init/1, terminate/3]).

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
	{ok, connected, Socket}.

% Nothing useful
connected(X, Pid, Info) ->
	io:format("connected/3 called~n", []).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Callbacks
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Auth request
handle_info({tcp, _Port, [$R|Data]}, connected, Info) ->
	{Len, Rest1} = getInt32(Data),
	{TypeNum, Rest2} = getInt32(Rest1),
	Type = case TypeNum of
		0 -> auth_ok;
		1 -> auth_k4;
		2 -> auth_k5;
		3 -> auth_clear;
		4 -> auth_crypt;
		5 -> auth_md5;
		6 -> auth_scm
	end,
	io:format("Got auth request (~p):  ~p bytes:  ~p~n", [Type, Len, Rest2]),
	{next_state, connected, Info}.

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
