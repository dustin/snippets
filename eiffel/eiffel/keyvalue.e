indexing
	description: "Key/value pair."
class KEYVALUE

creation make

feature

	make(k, v: STRING): is
		-- Make with a key and value pair
		do
			key:=k;
			value:=v;
		end;

	key: STRING is
		-- Get the key
		do
			Result:=key;
		end;

	value: STRING is
		-- Get the value
		do
			Result:=value;
		end;

feature {NONE}

	key: STRING;

	value: STRING;

end
