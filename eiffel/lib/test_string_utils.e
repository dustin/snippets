indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: test_string_utils.e,v 1.1 2002/11/14 08:18:08 dustin Exp $
--
class TEST_STRING_UTILS

creation {ANY}
	make

feature {ANY}

	instring: STRING is "Blah1 blah2-blah3"

	make is
		-- Instantiate TEST_STRING_UTILS
		local
			u: SPY_STRING_UTILS
			a, b: ARRAY[STRING]
		do
			!!u
			a := u.split_on(instring, '-')
			b := u.split_on(a @ 1, ' ')

			io.put_string("instring:  " + instring + "%N")
			io.put_string("a @ 1:  " + (a @ 1) + "%N")
			io.put_string("b @ 1:  " + (b @ 1) + "%N")
		end -- make

end -- class TEST_STRING_UTILS

