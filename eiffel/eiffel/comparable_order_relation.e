indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: comparable_order_relation.e,v 1.1 2002/11/13 09:42:10 dustin Exp $
--
class COMPARABLE_ORDER_RELATION[G->COMPARABLE]

inherit P_ORDER_RELATION[G] end

feature {P_CONTAINER}

	ordered(first, second: G) : BOOLEAN is
		do
			Result := first < second
		end

end -- class COMPARABLE_ORDER_RELATION[G->COMPARABLE]
