indexing
	description: "";
	version: "$Revision: 1.3 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: spy_time_utils.e,v 1.3 2002/12/08 10:06:38 dustin Exp $
--
class SPY_TIME_UTILS
	-- Time with my extensions

feature {ANY}
	-- Normal stuff

	parse(format, str: STRING): TIME is
		-- Parse the time base out of the given string.
		-- Currently, only one format is supported:
		-- "%Y-%m-%d %H:%M:%S"
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
			t: TIME
		once
			!!t
			t.set_universal_time
			if t.set(1970, 1, 1, 0, 0, 0) then
				Result := t
			end
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
