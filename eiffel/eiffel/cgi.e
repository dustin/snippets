indexing
	description: "CGI Processing routines."
class CGI creation
	make, parse

feature

	make is
		-- Standalone, we simply parse the data, and print it back out.
	local
		c: CHARACTER;
	do
		parse;
		display;
		c:=from_hex("41");
		c:=from_hex("2a");
	end

	display is
		-- Display our data.
	local
		i: INTEGER;
		kv: KEYVALUE;
	do
		from
			i:=1
		until
			i>cgi_data.count
		loop
			kv:=cgi_data @ i;
			io.put_string(kv.key);
			io.put_string(" -> ");
			io.put_string(kv.value);
			io.put_string("%N");
			i:=i+1;
		end;
	end

	parse is
		-- Parse the CGI data, only done once.
	local
		a, b: ARRAY[STRING];
		kv: KEYVALUE;
		s, s2: STRING;
		i: INTEGER;
	once
		s:=get_cgi_data;

		a:=s.split_on('&');
		-- Need room for cgi data
		!!cgi_data.with_capacity(1, 1);

		from
			i:=1
		until
			i>a.count
		loop
			s2:=a @ i;
			b:=s2.split_on('=');

			!!kv.make( decode_string(b.item(1)), decode_string(b.item(2)) );

			cgi_data.add_last(kv);

			i:=i+1;
		end
	end

	get_cgi_data: STRING is
		-- Find the CGI form data and return it.
		local
			tmp: STRING;
		once
			tmp:=get_environment_variable("REQUEST_METHOD");

			if tmp /= Void then
				if tmp.is_equal("GET") then
					Result:=get_environment_variable("QUERY_STRING");
				end
			end
		ensure
			Result /= Void
		end

feature
	-- Misc features, probably should be in a utility class

	decode_string(in: STRING): STRING is
		-- Decode an HTTP encoded string.
		local
			i: INTEGER;
		do
			!!Result.make(1);

			from
				i:=1;
			until
				i>in.count
			loop

				if(in.item(i) = '+') then
					Result.add_last(' ');
				elseif in.item(i) = '%%' then
					i:=i+1; -- skip %
					Result.add_last( from_hex(in.substring(i, i+2)) );
					i:=i+1; -- the last character will go below
				else
					Result.add_last(in.item(i));
				end

				i:=i+1;
			end
		end

	from_hex(in: STRING): CHARACTER is
		-- Take a two digit hex string, and convert it to a hex character.
		require
			(in.item(1)).is_hex_digit;
			(in.item(2)).is_hex_digit;
		local
			a, b: INTEGER;
			map, tmp: STRING;
		do
			!!tmp.copy(in);
			tmp.to_upper;

			map:="0123456789ABCDEF";
			a:= (map.index_of(tmp.item(1))) -1;
			b:= (map.index_of(tmp.item(2))) -1;

			a:=(a*16)+b;

			Result:=a.to_character;
		end

feature {NONE}

	cgi_data: ARRAY[KEYVALUE];

end
