indexing
	description: "Key/value pair."
	author: "Dustin Sallings <dustin@spy.net>";
	version: "$Revision: 1.3 $";
	copyright: "1999";
	license: "See forum.txt";


class KEYVALUE creation
	make

feature

	make(k, v: STRING) is
		-- Make with a key and value pair
		do
			key:=k;
			value:=v;
		end;

	key: STRING;

	value: STRING;

end
