indexing
	description: "Key/value pair."

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
