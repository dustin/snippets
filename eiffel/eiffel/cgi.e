indexing
	description: "CGI Processing routines."
class CGI creation
	make

feature

	make is
		-- print a simple message
	local
		a, b: ARRAY[STRING];
		kv: KEYVALUE;
		s, s2: STRING;
		i: INTEGER;
	do
		s:=argument(1);
		a:=s.split_on(':');
		-- Need room for cgi data
		!!cgi_data.with_capacity(1);

		from
			i:=1
		until
			i>a.count
		loop
			s2:=a @ i;
			b:=s2.split_on('=');

			!!kv.make(b@1, b@2);

			cgi_data.add_last(kv);

			i:=i+1;
		end

		io.put_string("Going through keys and values:%N");

		from
			i:=1
		until
			i>cgi_data.count
		loop
			kv:=cgi_data@i;

			io.put_string(kv.key);
			io.put_string(" -> ");
			io.put_string(kv.value);
			io.put_string("%N");
			i:=i+1;
		end;

	end

feature {NONE}

	cgi_data: ARRAY[KEYVALUE];

end
