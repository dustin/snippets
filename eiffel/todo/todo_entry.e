indexing
	description: "Todo entry";
	revision: "$Revision: 1.1 $";

class TODO_ENTRY creation
	from_array

feature

	from_array(in: ARRAY[STRING]) is
		require
			in.count=10;
		local
			i: INTEGER;
		do

			i:=in.lower;

			user_name:=   in.item(i); i:=i+1;
			task_id:=     in.item(i); i:=i+1;
			user_id:=     in.item(i); i:=i+1;
			earliest:=    in.item(i); i:=i+1;
			latest:=      in.item(i); i:=i+1;
			priority:=    in.item(i); i:=i+1;
			summary:=     in.item(i); i:=i+1;
			description:= in.item(i); i:=i+1;
			finished:=    in.item(i); i:=i+1;
			created:=     in.item(i); i:=i+1;

		end

	user_name: STRING;

	task_id: STRING;

	user_id: STRING;

	earliest: STRING;

	latest: STRING;

	priority: STRING;

	summary: STRING;

	description: STRING;

	finished: STRING;

	created: STRING;

end
