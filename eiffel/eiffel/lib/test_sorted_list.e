indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: test_sorted_list.e,v 1.1 2002/11/14 08:18:08 dustin Exp $
--
class TEST_SORTED_LIST

creation {ANY}
	make

feature {ANY}

	make is
		-- Instantiate TEST_SORTED_LIST
		local
			l: SORTED_ARRAY[STRING]
		do
			!!l.make(0, -1)
			l.add_last("Z")
			l.add_last("B")
			l.add_last("A")
			l.add_last("C")

			dump(l)
		end -- make

feature {NONE}

	dump(a: SORTED_ARRAY[STRING]) is
		require
			valid_iterator: a /= Void;
		local
			it: ITERATOR_ON_COLLECTION[STRING]
		do
			!!it.make(a)
			io.put_string("Printing out the list.%N")
			from
				it.start
			until
				it.is_off
			loop
				io.put_string(it.item.out + "%N")
				it.next
			end
		end

end -- class TEST_SORTED_LIST

