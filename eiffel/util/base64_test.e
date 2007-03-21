indexing
   description: "Base64 test.";
class BASE64_TEST

inherit
	EXCEPTIONS

insert
	ARGUMENTS

creation {ANY} 
   make

feature {ANY} -- Constructors

   make is 
      -- Initialization
      local 
         -- e, d: STRING;
		 b: BASE64
      do  
         !!b
	     test_cases.do_all(agent test_decode(?,?))
		 io.put_string("Completed tests.%N")
      end -- make

   test_decode(k: STRING; v: STRING) is
   local
   	b: BASE64
   do
	!!b
	if not b.encode(v).is_equal(k)
	then
		raise("Failed encode of " + v + "''")
	end
	if not b.decode(k).is_equal(v)
	then
		raise("Failed decode of ``" + k + "''")
	end
   end

   test_cases: HASHED_DICTIONARY[STRING, STRING]
   	is
	once
		!!Result.make
		Result.put("d3d3LnB5dGhvbi5vcmc=", "www.python.org")
		Result.put("YQ==", "a")
		Result.put("YWI=", "ab")
		Result.put("YWJj", "abc")
		Result.put("", "")
		Result.put("YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXpBQ"
			+ "kNERUZHSElKS0xNTk9QUVJTVFVWV1hZWjAxMjM0%N"
			+ "NTY3ODkhQCMwXiYqKCk7Ojw+LC4gW117fQ==",
			"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
			+ "0123456789!@#0^&*();:<>,. []{}")
	end
end
