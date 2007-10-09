-module(mnesia_test).
-export([create/0, create/1, add_thing/2, get_thing/1]).

-record(thing, {thing_id, thing_name }).

% Create the test database on the given set of nodes.
create(NodeList) ->
	mnesia:create_schema(NodeList),
	mnesia:start(),
	mnesia:create_table(thing, [
			{attributes, record_info(fields, thing)},
			{disc_copies, NodeList}
		]).

% Create the test database within the current node
create() -> create([node()]).

% Add a thing to the test DB
add_thing(Id, Name) ->
	mnesia:activity(transaction, fun () ->
			mnesia:write(#thing{thing_id = Id, thing_name = Name})
		end).

% Get a thing from the test DB
get_thing(Id) ->
	mnesia:activity(transaction, fun () -> mnesia:read({thing, Id}) end).
