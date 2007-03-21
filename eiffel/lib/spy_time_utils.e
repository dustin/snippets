indexing
	description: "";
	version: "$Revision: 1.4 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

-- 
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: spy_time_utils.e,v 1.4 2002/12/08 23:42:08 dustin Exp $
--
class SPY_TIME_UTILS
	-- Time with my extensions

inherit
	SPY_STRING_UTILS

feature {NONE}
	-- Private stuff

	stu_tparts: ARRAY[STRING] is
		-- Time parts
		once
			!!Result.with_capacity(4,1)
		end

	stu_dparts: ARRAY[STRING] is
		-- Time parts
		once
			!!Result.with_capacity(8,1)
		end

	stu_strparts: ARRAY[STRING] is
		-- All parts of the string
		once
			!!Result.with_capacity(2,1)
		end

feature {ANY}
	-- Normal stuff

	parse(format, s: STRING): TIME is
		-- Parse the time base out of the given string.
		-- Currently, only one format is supported:
		-- "%Y-%m-%d %H:%M:%S"
		require
			known_format: format.is_equal("%%Y-%%m-%%d %%H:%%M:%%S")
		local
			rv: BOOLEAN
		do
			!!Result

			stu_strparts.clear
			stu_dparts.clear
			stu_tparts.clear
			split_on_into(s, ' ', stu_strparts)
			split_on_into(stu_strparts @ 1, '-', stu_dparts)
			split_on_into(stu_strparts @ 2, ':', stu_tparts)

			-- OK, set it here
			rv := Result.set((stu_dparts @ 1).to_integer,
				(stu_dparts @ 2).to_integer,
				(stu_dparts @ 3).to_integer,
				(stu_tparts @ 1).to_integer,
				(stu_tparts @ 2).to_integer,
				(stu_tparts @ 3).to_integer)

			check rv end
		end -- make

	to_unix_time(t: TIME): INTEGER is
		require
			valid_time: t /= Void
			after_1970: t >= epoch
		do
			Result := epoch.elapsed_seconds(t)
		ensure
			Result /= Void
		end

	epoch: TIME is
		-- Time representing the UNIX epoch
		local
			rv: BOOLEAN
		once
			!!Result
			Result.set_universal_time
			rv := Result.set(1970, 1, 1, 0, 0, 0)

			-- Verify the set worked
			check rv end
		ensure
			Result /= Void
		end

feature{NONE}
	-- Debug stuff.

 	dump_time(t: TIME) is
		-- Dump out the time
 		local
 			tie: TIME_IN_ENGLISH
 		do
 			!!tie
 			tie.set_time(t)
 
 			io.put_string(tie.to_string + "%N")
 		end

end -- class SPY_TIME_UTILS
