indexing
	description: "Hello, damnit!"
class HELLO creation
	make

feature

	make is
		-- print a simple message
	do
		io.put_string("Hello, damnit...%N");
	end
end
