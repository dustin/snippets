indexing
   description: "Base64 test.";
class BASE64_TEST

insert
	ARGUMENTS

creation {ANY}
   make

feature {ANY} -- Constructors

   make is
      -- Initialization
      local
         e, d: STRING;
		 b: BASE64;
      do
         !!b;
         io.put_string("Encoded:%N");
         e:=b.encode(argument(1));
         io.put_string(e);
         io.put_string("%N");

         io.put_string("Decoded:%N");
         d:=b.decode(e);
         io.put_string(d);
         io.put_string("%N");
      end -- make
end
