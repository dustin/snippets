indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: spy_time_utils.e,v 1.1 2002/11/14 22:09:42 dustin Exp $
--
class SPY_TIME_UTILS
	-- Time with my extensions

creation {ANY}
	make

feature {ANY}
	-- Creation

	make is
		-- Make with an initial capacity
		local
			t: TIME
		do
			t:=parse("%%Y-%%m-%%d %%H:%%M:%%S", "2002-11-13 10:30:05")
			dump_time(t)
		end

	parse(format, str: STRING): TIME is
		-- Parse the time base out of the given string.
		-- Currently, only one format is supported.
		require
			known_format: format.is_equal("%%Y-%%m-%%d %%H:%%M:%%S")
		local
			u: SPY_STRING_UTILS
			s: STRING
			strparts: ARRAY[STRING]
			tparts, dparts: ARRAY[STRING]
			rv: BOOLEAN
		do
			!!s.copy(str)
			!!u
			!!Result

			strparts := u.split_on(s, ' ')
			dparts := u.split_on(strparts @ 1, '-')
			tparts := u.split_on(strparts @ 2, ':')

			-- OK, set it here
			rv := Result.set((dparts @ 1).to_integer,
				(dparts @ 2).to_integer,
				(dparts @ 3).to_integer,
				(tparts @ 1).to_integer,
				(tparts @ 2).to_integer,
				(tparts @ 3).to_integer)

			check
				rv
			end
		end -- make

feature{NONE}
	-- Debug stuff.

	dump_time(t: TIME) is
		local
			tie: TIME_IN_ENGLISH
		do
			!!tie
			tie.set_time(t)

			io.put_string(tie.to_string + "%N")
		end

end -- class SPY_TIME_UTILS
