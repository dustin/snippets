indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: test_sorted_list.e,v 1.1 2002/11/13 09:42:11 dustin Exp $
--
class TEST_SORTED_LIST

creation {ANY}
	make

feature {ANY}

	make is
		-- Instantiate TEST_SORTED_LIST
		local
			l: SORTED_LIST[STRING]
		do
			!!l.make
			l.add("B")
			l.add("A")
			l.add("C")
			l.sort

			dump(l.iterator)
		end -- make

feature {NONE}

	dump(it: P_ITERATOR[STRING]) is
		require
			valid_iterator: it /= Void;
		do
			io.put_string("Printing out the list.%N")
			from
				it.first
			until
				it.outside
			loop
				io.put_string(it.item.out + "%N")
				it.forth
			end
		end

end -- class TEST_SORTED_LIST

