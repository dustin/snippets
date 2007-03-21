indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

-- 
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: sorted_array.e,v 1.1 2002/11/14 07:49:29 dustin Exp $
--
class SORTED_ARRAY[G->COMPARABLE]
	-- An array that stays sorted.

inherit ARRAY[G]
	rename add_last as array_add_last
	end

creation
	make, with_capacity, from_collection

feature{NONE}

	sorter: COLLECTION_SORTER[G];

feature{ANY}
	-- Add override

	add_last(ob: G) is 
		do
			sorter.add(Current, ob)
		end

end -- class SORTED_ARRAY[G->COMPARABLE]
