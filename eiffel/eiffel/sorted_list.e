indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: sorted_list.e,v 1.1 2002/11/13 09:42:10 dustin Exp $
--
class SORTED_LIST[G->COMPARABLE]
	inherit P_SEQUENCE_LIST[G]
		undefine make
		end

creation
	make

feature{NONE}

	make is
		local
			orel: COMPARABLE_ORDER_RELATION[G]
		do
			reset
			!!orel
			set_order(orel)
		end

end -- class SORTED_LIST[G->COMPARABLE]

