indexing
   description: "Temperature loader."
--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: temploader.e,v 1.9 2002/12/08 10:46:37 dustin Exp $
--
class TEMPLOADER

inherit
	LOGGER redefine make end
	LINE_PROCESSOR

creation {ANY}
   make

feature {ANY}

	make is
		-- Process the tempearture log from stdin.  Database name is the
		-- first system argument, if provided.
		local
			dbserver: STRING
		do
			-- Get the name of the database server from the commandline
			if argument_count > 0 then
				dbserver := argument(1)
			else
				dbserver := "db"
			end

			!!db.make
			db.set_dbname("temperature")
			db.set_username("tempload")
			db.set_password("tempload")
			db.set_host(dbserver)
			db.set_max_retries(0)
			db.connect

			init_sensors
			-- Process the lines
			set_input_stream(io)
			process_input
		end

feature{NONE}

	db: PG

	serials: DICTIONARY[INTEGER,STRING]

	init_sensors is
		-- Initialize the sensors map
		local
			b: BOOLEAN
			a: ARRAY[STRING]
			ser, id: STRING
		once
			db.query("select sensor_id, serial from sensors")
			!!serials.make
			from
				-- Get the first row
				b := db.get_row
			until
				b = false
			loop
				a := db.last_row

				ser := a @ 1
				id := a @ 0

				-- io.put_string(ser + " = " + id + "%N")

				-- get the ID and the serial and map it
				serials.put(id.to_integer, ser)

				-- Get the next row
				b := db.get_row
			end
		end

	process_line(line: STRING) is
		local
			done: BOOLEAN
			string_utils: SPY_STRING_UTILS
			a: ARRAY[STRING]
			tsa: ARRAY[STRING]
			query: STRING
		do
			-- This allows failures to pass
			if not done then
				done := true

				!!string_utils
				a := string_utils.split_on(io.last_string, '%T')
				tsa := string_utils.split_on( (a @ 1), '.')

				-- Print a message
				info_msg(line_number.to_string
					+ ": " + (tsa @ 1) + "%T" + (a @ 2) + "=" + (a @ 3))

				-- Build the query string
				query := "insert into samples(ts, sensor_id, sample) "
					+ "values('" + (tsa @ 1) + "', "
					+ (serials @ (a @ 2)).to_string
					+ ", " + (a @ 3) + ")%N"

				-- Perform the insert
				db.query(query)
				-- Clear the results
				db.clear_results
			end
		rescue
			if query /= Void then
				error_msg("FAILED:" + line_number.to_string
					+ "(DB):  " + query + db.errmsg)
			else
				error_msg("FAILED:" + line_number.to_string
					+ "(Unknown):  " + line)
			end
			retry
		end

end -- class TEMPLOADER
