indexing
	description: "Square thingy"
class SQUARE creation
	make

feature

	square (x: INTEGER): INTEGER is
		require
			positive: x>=0;
		do
			Result:= x * x;
		ensure
			big_enough: Result > 0;
		end

	make is
		-- print a simple message
		local
			n, i: INTEGER;
		do

			i:=2;
			n:=square(i);

			io.put_string("1^" );
			io.put_integer(i);
			io.put_string(" is ");
			io.put_integer(n);
			io.put_string("%N");
		end
end
