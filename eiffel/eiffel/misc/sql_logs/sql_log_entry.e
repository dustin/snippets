indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: sql_log_entry.e,v 1.1 2002/12/07 10:07:43 dustin Exp $
--
class SQL_LOG_ENTRY

inherit GENERAL
	redefine out
	end

creation {ANY}
	make, parse_string

feature {NONE}

	line: STRING
		-- The log line

	timestamp: TIME
		-- The log timestamp

feature {ANY}
	-- Accessing the parsed parts

	calls: INTEGER
		-- The number of calls

	call_time: INTEGER
		-- The amount of time (in milliseconds) in those calls.

	out: STRING is
		local
			tie: TIME_IN_ENGLISH
		do
			!!tie
			tie.set_time(timestamp)

			Result := call_time.out + "ms for " + calls.out + " queries at "
				+ tie.to_string
		end

feature {NONE}
	-- Private creation
	example: STRING is "2002-12-05 00:00:08,422 w51-1  INFO database.DBManager.sql 479-772-MuxChannel-2 wayBean.processHeartbeatData(:3974) CMS     1  0s    10ms/3 SELECT Owner_IDnum_pk, Organization_num_fk FROM tbl_Owner WHERE Owner_IDnum_pk = 80149"

	make is
		local
			e: SQL_LOG_ENTRY
			tie: TIME_IN_ENGLISH
		do
			!!e.parse_string(example)
			!!tie
			tie.set_time(e.timestamp_tominute)
			io.put_string(e.out + "%N")
			io.put_string("Nearest minute:  " + tie.to_string + "%N")
		end

feature {ANY}
	-- Creation

	parse_string(l: STRING) is
		-- Instantiate SQL_LOG_ENTRY
		require
			is_sql_log: l.has_substring("database.DBManager.sql")
		local
			su: SPY_STRING_UTILS
			tu: SPY_TIME_UTILS
			parts: ARRAY[STRING]
			tparts: ARRAY[STRING]
			t: STRING
		do
			!!su
			!!tu

			-- Copy the line into the object
			line := l

			-- Get the time parts first
			tparts := su.split_on(l, ',')

			-- get the timestamp
			timestamp := tu.parse("%%Y-%%m-%%d %%H:%%M:%%S", (tparts @ 1))

			-- Split it on spaces
			parts := su.split_on(l, ' ')

			-- Grab the timings
			tparts := su.split_on( (parts @ 11), '/')

			-- Get the time spent
			t := tparts @ 1
			t.remove_substring(t.count - 1, t.count)

			call_time := t.to_integer
			calls := (tparts @ 2).to_integer
		ensure
			initialized: line /= Void and timestamp /= Void
							and call_time /= Void and calls /= Void
		end -- make

	timestamp_tominute: TIME is
		-- Get the timestamp to the nearest minute
		local
			t: TIME
		do
			t := timestamp.twin
			if t.set(timestamp.year,
						timestamp.month,
						timestamp.day,
						timestamp.hour,
						timestamp.minute,
						0)
			then
				Result := t
			end
		ensure
			successful: Result /= Void
		end

end -- class SQL_LOG_ENTRY
