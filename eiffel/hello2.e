indexing
	description: "Hello, damnit!"
class HELLO2 creation
	make

feature

	make is
		-- print a simple message
	local
		s: STRING;
		a: ARRAY[STRING];
		i: INTEGER;
		sys: SYSTEM;
	do
		-- Print out the path after splitting
		s:=sys.get_environment_variable("PATH");
		io.put_string(s);
		io.put_string("%N");
		a:=s.split;

		from
			i:=1;
		until
			i>a.count
		loop
			io.put_integer(i);
			io.put_string("%T");
			io.put_string(a @ i);
			io.put_string("%N");
			i:=i+1;
		end;
	end
end
