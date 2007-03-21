indexing
   description: "Postgres database access..."
   version: "$Revision: 1.25 $"
   author: "Dustin Sallings <dustin@spy.net>"
   copyright: "1999"
   license: "See forum.txt."

--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pg.e,v 1.25 2002/11/23 09:59:50 dustin Exp $
--
class PG

inherit 
   MEMORY
      redefine dispose
      end
   
creation {ANY} 
   make

feature {PG} -- Creation is private

   make is 
      -- Make an uninitialized PG object.
      do  
         current_row := 0
         !!last_row.make(0,16)
         last_row.clear_count
         c_buffer := c_buffer.calloc(c_buffer_len)
         conn := nullpointer
		 -- Default number of retries
		 max_retry_attempts := 3
      end -- make

feature {ANY} -- Connection options

   set_host(to: STRING) is 
      -- Set database host to connect to
      do  
         !!host.copy(to)
      end -- set_host
   
   set_port(to: STRING) is 
      -- Set database port to connect to
      do  
         !!port.copy(to)
      end -- set_port
   
   set_options(to: STRING) is 
      -- Set database connection options
      do  
         !!options.copy(to)
      end -- set_options
   
   set_tty(to: STRING) is 
      -- Set database tty
      do  
         !!tty.copy(to)
      end -- set_tty
   
   set_dbname(to: STRING) is 
      -- Set database to connect to
      do  
         !!dbname.copy(to)
      end -- set_dbname
   
   set_username(to: STRING) is 
      -- Set username to connect as
      do  
         !!username.copy(to)
      end -- set_username
   
   set_password(to: STRING) is 
      -- Set password for authentication
      do  
         !!password.copy(to)
      end -- set_password
   
   connect is 
      -- Make a database connection
      local 
         h, p, o, t, d, u, pass: POINTER
         retry_attempts: INTEGER
      do  
         if is_not_connected then 
            if host /= Void then 
               h := host.to_external
            end
            if port /= Void then 
               p := port.to_external
            end
            if options /= Void then 
               o := options.to_external
            end
            if tty /= Void then 
               t := tty.to_external
            end
            if dbname /= Void then 
               d := dbname.to_external
            end
            if username /= Void then 
               u := username.to_external
            end
            if password /= Void then 
               pass := password.to_external
            end
            conn := pg_connect(h,p,o,t,d,u,pass)
            check 
               pg_connection_ok(conn)
            end
         end
      ensure 
         is_connected: is_connected
      rescue 
         if retry_attempts < max_retry_attempts then 
            retry_attempts := retry_attempts + 1
            retry
         end
      end -- connect

feature {ANY} -- Query features

	set_max_retries(to: INTEGER) is
		-- Set the maximum number of retries per query
		require
			valid_integer: integer_is_between(to, 0, 100)
		do
			max_retry_attempts := to
		end

	integer_is_between(i, low, high: INTEGER): BOOLEAN is
		-- Make sure the given number is in the given range (inclusive)
		do
			Result := (i >= low) and (i <= high)
		end

   query(q: STRING) is 
      -- Query on an open database connection
      require 
         is_connected: is_connected
         has_query: q /= Void
      local 
         retry_attempts: INTEGER
      do  
         current_row := 0
         debug 
            io.put_string("PG: Doing query:  " + q)
         end
         res := pg_query(conn,q.to_external)
         debug 
            io.put_string("%NPG: -------------------------%N")
         end
      ensure 
         query_is_successful: query_successful
      rescue 
		 -- Free the results
		 if has_results then
		 	clear_results
		 end
         if retry_attempts < max_retry_attempts then 
            retry_attempts := retry_attempts + 1
            retry
         end
      end -- query
   
   num_rows: INTEGER is 
      -- Number of rows returned from the last query
      require 
         has_results: has_results
      do  
         Result := pg_ntuples(res)
      end -- num_rows

   clear_results is
	-- Destroy the current results
	require
		has_results: has_results
   	do
		pg_clear_result(res)
		res := nullpointer
	end
   
   get_row: BOOLEAN is 
      -- Get the next row of data back, returns False if there's no more data
      require 
         has_results: has_results
      local 
         i, fields: INTEGER
         s: STRING
         p: POINTER
      do  
         if current_row >= num_rows then 
			clear_results
            Result := False
         else 
            from 
               fields := pg_nfields(res)
               last_row.clear_count
               i := 0
            until 
               i >= fields
            loop 
               p := pg_intersect(res,current_row,i)
               !!s.from_external_copy(p)
               last_row.add_last(s)
               i := i + 1
            end
            current_row := current_row + 1
            Result := True
         end
      end -- get_row

feature {ANY} -- Copy stuff

   copy_to(table: STRING) is 
      -- Begin a copy to the database.
      require 
         is_connected: is_connected
         has_table: table /= Void
      local 
         q: STRING
      do  
         q := "copy " + table + " from STDIN"
         query(q)
         tocopy := True
      ensure 
         copy_in_ready: copy_in_ready
      end -- copy_to
   
   copy_from(table: STRING) is 
      -- Begin a copy from the database.
      require 
         is_connected: is_connected
         has_table: table /= Void
      local 
         q: STRING
      do  
         q := "copy " + table + " to STDOUT"
         query(q)
      ensure 
         copy_out_ready: copy_out_ready
      end -- copy_from
   
   putline(line: STRING) is 
      -- Send a line to the database.
      require 
         has_line: line /= Void
         is_connected: is_connected
      do  
         debug 
            io.put_string("PG-Line: " + line)
         end
         check 
            pg_putline(conn,line.to_external) = 0
         end
      rescue 
         io.put_string("Line error:  " + host + " " + errmsg + "%N")
      end -- putline
   
   last_line: STRING
      -- last line from getline
   
   getline: BOOLEAN is 
      -- Get a line from the database.
      require 
         is_connected: is_connected
      local 
         size: INTEGER
      do  
         size := pg_getline(conn,c_buffer.to_external,c_buffer_len)
         !!last_line.from_external_copy(c_buffer.to_external)
         Result := not last_line.is_equal("\.") or size < 0
         debug 
            if Result then 
               io.put_string("PG-Line: Got -> " + last_line + "%N")
            else 
               io.put_string("PG-Line: End")
            end
         end
      end -- getline
   
   endcopy is 
      -- End a copy to/from
      require 
         is_connected: is_connected
      local 
         tmp: STRING
      do  
         if tocopy then 
            debug 
               io.put_string("PG: Ending TO copy%N")
            end
            !!tmp.copy("\.%N")
            debug 
               io.put_string("PG-Line: " + tmp)
            end
            check 
               pg_putline(conn,tmp.to_external) = 0
            end
            tocopy := False
         else 
            debug 
               io.put_string("PG: Ending FROM copy%N")
            end
         end
         debug 
            io.put_string("PG: Ending copy.%N")
         end
         check 
            pg_endcopy(conn) = 0
         end
      end -- endcopy

feature {ANY} -- Transaction

   begin is 
      require 
         is_connected: is_connected
      do  
         query("begin transaction")
      end -- begin
   
   commit is 
      require 
         is_connected: is_connected
      do  
         query("commit")
      end -- commit
   
   rollback is 
      require 
         is_connected: is_connected
      do  
         query("rollback")
      end -- rollback

feature {ANY} -- Utility

   quote(s: STRING): STRING is 
      -- Quote a string for safety.
      require 
         has_string: s /= Void
      local 
         tmp: STRING
         i: INTEGER
      do  
         !!tmp.copy("'")
         if s.index_of('%'', 1) < s.count then 
            -- We only need to do this slow copy if we've got a quote
            from 
               i := 1
            until 
               i > s.count
            loop 
               if s.item(i) = '%'' then 
                  tmp.append_character('%'')
               end
               tmp.append_character(s.item(i))
               i := i + 1
            end
         else 
            tmp.append(s)
         end
         tmp.append("'")
         Result := tmp
      end -- quote

feature {ANY} -- Database Information

   tables: ARRAY[STRING] is 
      -- List all tables in this database.
      require 
         is_connected: is_connected
      local 
         a: ARRAY[STRING]
         b: BOOLEAN
         q: STRING
      do  
         !!Result.with_capacity(0,16)
         Result.clear_count
         q := "select tablename from pg_tables " +
		 	"where tablename not like 'pg_%%'"
		 q := "select C.relname from pg_class C where relkind = 'r' " +
			" and C.relname not like 'pg_%%' " +
		 	" and not exists (select rulename from pg_rewrite where " +
			" ev_class = C.oid and ev_type = '1') "
         query(q)
         from 
            b := get_row
         until 
            b = False
         loop 
            a := last_row
            Result.add_last(a @ 0)
            b := get_row
         end
      ensure 
         has_results: Result /= Void
      end -- tables
   
   sequences: ARRAY[STRING] is 
      -- List all sequences in this database.
      require 
         is_connected: is_connected
      local 
         a: ARRAY[STRING]
         b: BOOLEAN
         q: STRING
      do  
         !!Result.with_capacity(0,16)
         Result.clear_count
         !!q.copy("select * from pg_class where relkind='S'")
         query(q)
         from 
            b := get_row
         until 
            b = False
         loop 
            a := last_row
            Result.add_last(a @ 0)
            b := get_row
         end
      ensure 
         has_results: Result /= Void
      end -- sequences

feature {ANY} -- Status

   is_connected: BOOLEAN is 
      -- Find out if we're connected.
      do  
         Result := conn.is_not_null
      end -- is_connected
   
   is_not_connected: BOOLEAN is 
      -- Find out if we're not connected (shortcut)
      do  
         Result := not is_connected
      end -- is_not_connected
   
   has_results: BOOLEAN is 
      -- Find out if we have results
      do  
         Result := res.is_not_null
      end -- has_results
   
   query_successful: BOOLEAN is 
      -- Find out if the query was successful
      require 
         has_results: has_results
      do  
         Result := not (pg_nonfatal_error(res) or pg_fatal_error(res) or pg_bad_response(res))
      end -- query_successful
   
   copy_in_ready: BOOLEAN is 
      -- Are we ready for a copy in?
      require 
         has_results: has_results
      do  
         Result := pg_copy_in(res)
      end -- copy_in_ready
   
   copy_out_ready: BOOLEAN is 
      -- Are we ready for a copy out?
      require 
         has_results: has_results
      do  
         Result := pg_copy_out(res)
      end -- copy_out_ready
   
   errmsg: STRING is 
      -- Get the last error message.
      require 
         is_connected: is_connected
      do  
         !!Result.from_external_copy(pg_errmsg(conn))
      end -- errmsg

feature {ANY} -- Available data

   current_row: INTEGER
      -- Current row number we're on.
   
   last_row: ARRAY[STRING]
      -- Last row retrieved.
   
feature {PG} -- Internal data stuff

   max_retry_attempts: INTEGER
      -- number of times to retry on exception
   
   conn: POINTER
      -- Connection holder for C library.
   
   res: POINTER
      -- Result holder for C library.
   
   nullpointer: POINTER
      -- Result holder for C library.
   
   c_buffer: NATIVE_ARRAY[CHARACTER]
      -- buffer for C
   
   c_buffer_len: INTEGER is 4096
      -- length of buffer for C
   
   tocopy: BOOLEAN
      -- Are we doing a to_copy?
   
   host: STRING
      -- Database host
   
   port: STRING
      -- Database port
   
   options: STRING
      -- Database options
   
   tty: STRING
      -- Database tty
   
   dbname: STRING
      -- Database name
   
   username: STRING
      -- Database username
   
   password: STRING
      -- Database password

feature {ANY} -- Destructor

   dispose is 
      do  
         if conn /= nullpointer then 
            debug 
               io.put_string("PG: Closing database connection.%N")
            end
            pg_finish(conn)
            conn := nullpointer
         end
      end -- dispose

feature {PG} -- Constants from pq

   pg_connection_ok(c: POINTER): BOOLEAN is 
      external "C"
      end -- pg_connection_ok
   
   pg_connection_bad(c: POINTER): BOOLEAN is 
      external "C"
      end -- pg_connection_bad
   
   pg_empty_query(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_empty_query
   
   pg_command_ok(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_command_ok
   
   pg_tuples_ok(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_tuples_ok
   
   pg_copy_out(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_copy_out
   
   pg_copy_in(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_copy_in
   
   pg_bad_response(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_bad_response
   
   pg_nonfatal_error(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_nonfatal_error
   
   pg_fatal_error(r: POINTER): BOOLEAN is 
      external "C"
      end -- pg_fatal_error

feature {PG} -- C bindings

   pg_result_status(r: POINTER): INTEGER is 
      external "C"
      alias "PQresultStatus"
      end -- pg_result_status
   
   pg_result_status_string(of: INTEGER): POINTER is 
      external "C"
      alias "PQresStatus"
      end -- pg_result_status_string
   
   pg_connect(h, p, o, t, d, u, pass: POINTER): POINTER is 
      external "C"
      alias "PQsetdbLogin"
      end -- pg_connect
   
   pg_query(c, q: POINTER): POINTER is 
      external "C"
      alias "PQexec"
      end -- pg_query
   
   pg_intersect(r: POINTER; i, j: INTEGER): POINTER is 
      external "C"
      alias "PQgetvalue"
      end -- pg_intersect
   
   pg_ntuples(r: POINTER): INTEGER is 
      external "C"
      alias "PQntuples"
      end -- pg_ntuples
   
   pg_nfields(r: POINTER): INTEGER is 
      external "C"
      alias "PQnfields"
      end -- pg_nfields
   
   pg_clear_result(r: POINTER) is 
      external "C"
      alias "PQclear"
      end -- pg_clear_result
   
   pg_finish(r: POINTER) is 
      external "C"
      alias "PQfinish"
      end -- pg_finish
   
   pg_putline(c, r: POINTER): INTEGER is 
      external "C"
      alias "PQputline"
      end -- pg_putline
   
   pg_getline(c, r: POINTER; length: INTEGER): INTEGER is 
      external "C"
      alias "PQgetline"
      end -- pg_getline
   
   pg_endcopy(c: POINTER): INTEGER is 
      external "C"
      alias "PQendcopy"
      end -- pg_endcopy
   
   pg_errmsg(c: POINTER): POINTER is 
      external "C"
      alias "PQerrorMessage"
      end -- pg_errmsg

end -- class PG
