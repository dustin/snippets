indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

-- 
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: sql_log.e,v 1.1 2002/12/08 23:44:28 dustin Exp $
--
class SQL_LOG

inherit
	GENERAL redefine out end
	LINE_PROCESSOR
	SPY_TIME_UTILS rename parse as parse_time end

creation {ANY}
	make, parse_string

feature {NONE}
	-- Private features for the entry itself

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
	-- Private stuff for parsing files

	total_calls: INTEGER
		-- Total number of calls so far
	total_time: INTEGER
		-- Total processing time so far
	last_time: INTEGER
		-- Last entry time

	make is
		do
			last_time := 0
			-- io.put_string(e.out + "%N")

			-- Begin the processing
			set_input_stream(io)
			process_input
		end

	process_line(line: STRING) is
		local
			done: BOOLEAN
			entry: SQL_LOG
			tmp_time: INTEGER
			div_call_time: INTEGER
		do
			if not done and is_sql_log(line) then
				done := true
				!!entry.parse_string(line)

				-- Only process things that represent more than one call
				if entry.calls > 0 then

					-- Make sure there's a timestamp
					if last_time = 0 then
						last_time := to_unix_time(entry.timestamp_tominute)
					end

					-- Get the closest curent time
					tmp_time := to_unix_time(entry.timestamp_tominute)

					-- If this is a new timestamp, spit out the current stats
					if tmp_time /= last_time then
						io.put_string("update some.rrd "
							+ last_time.to_string
							+ ":" + total_calls.to_string
							+ ":" + total_time.to_string + "%N")
						total_calls := 0
						total_time := 0
					end

					last_time := tmp_time
					total_calls := total_calls + 1
					div_call_time := (entry.call_time / entry.calls).floor
					total_time := total_time + div_call_time
				end -- An entry with more than 0 calls
			end -- not done and is log
		end

feature {NONE}
	-- Save some object creation

	p_tparts: ARRAY[STRING] is
		-- Time parts
		once
			!!Result.with_capacity(4,1)
		end

	p_parts: ARRAY[STRING] is
		-- parts of the line
		once
			!!Result.with_capacity(64,1)
		end

feature {ANY}
	-- Creation

	parse_string(l: STRING) is
		-- Instantiate SQL_LOG
		require
			is_sql_log: is_sql_log(l)
		local
			t: STRING
		do

			-- Get the time parts first
			p_tparts.clear
			split_on_into(l, ',', p_tparts)

			-- get the timestamp
			timestamp := parse_time("%%Y-%%m-%%d %%H:%%M:%%S", (p_tparts @ 1))

			-- Split it on spaces
			p_parts.clear
			split_on_into(l, ' ', p_parts)

			-- Grab the timings
			p_tparts.clear
			split_on_into( (p_parts @ 11), '/', p_tparts)

			-- Get the time spent
			t := p_tparts @ 1
			t.remove_substring(t.count - 1, t.count)

			call_time := t.to_integer
			calls := (p_tparts @ 2).to_integer
		ensure
			initialized: timestamp /= Void
							and call_time /= Void and calls /= Void
		end -- make

feature {ANY}
	-- Working with entries

	is_sql_log(s: STRING): BOOLEAN is
		-- True if the passed string represents a SQL LOG entry.
		require
			has_string: s /= Void
		do
			Result := s.has_substring("database.DBManager.sql")
		end

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

end -- class SQL_LOG
