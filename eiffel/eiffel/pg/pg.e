indexing
   description: "Postgres database access...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pg.e,v 1.6 1999/05/26 01:51:43 dustin Exp $
--
class PG

creation {ANY}
   make

feature {ANY}

   make is
      -- Make an uninitialized PG object.
      do
         current_row := 0;
      end -- make

   current_row: INTEGER;
      -- Current row number we're on.

   last_row: ARRAY[STRING];
      -- Last row retrieved.

   connect: BOOLEAN is
      -- Make a database connection
      local
         h, p, o, t, d, u, pass: POINTER;
      do
         if host /= Void then
            h := host.to_external;
         end;
         if port /= Void then
            p := port.to_external;
         end;
         if options /= Void then
            o := options.to_external;
         end;
         if tty /= Void then
            t := tty.to_external;
         end;
         if dbname /= Void then
            d := dbname.to_external;
         end;
         if username /= Void then
            u := username.to_external;
         end;
         if password /= Void then
            pass := password.to_external;
         end;
         conn := pg_connect(h,p,o,t,d,u,pass);
         if conn = Void then
            Result := false;
         else
            Result := true;
         end;
      end -- connect

   query(q: STRING): BOOLEAN is
      -- Query on an open database connection
      require
		 is_connected;
      do
         res := pg_query(conn,q.to_external);
         if res = Void then
            Result := false;
         else
            Result := true;
         end;
      end -- query

   get_row: BOOLEAN is
      -- Get the next row of data back
      require
		has_results;
      local
         i, fields: INTEGER;
         s: STRING;
         p: POINTER;
      do
         if current_row >= pg_ntuples(res) then
            Result := false;
            pg_finish(conn);
         else
            from
               fields := pg_nfields(res);
               !!last_row.make(0,0);
               last_row.clear;
               i := 0;
            until
               i >= fields
            loop
               p := pg_intersect(res,current_row,i);
               !!s.from_external(p);
               last_row.add_last(s);
               i := i + 1;
            end;
            current_row := current_row + 1;
            Result := true;
         end;
      end -- get_row

feature {ANY} -- Connection options

   set_host(to: STRING) is
      -- Set database host to connect to
      do
         !!host.copy(to);
      end -- set_host

   set_port(to: STRING) is
      -- Set database port to connect to
      do
         !!port.copy(to);
      end -- set_port

   set_options(to: STRING) is
      -- Set database connection options
      do
         !!options.copy(to);
      end -- set_options

   set_tty(to: STRING) is
      -- Set database tty
      do
         !!tty.copy(to);
      end -- set_tty

   set_dbname(to: STRING) is
      -- Set database to connect to
      do
         !!dbname.copy(to);
      end -- set_dbname

   set_username(to: STRING) is
      -- Set username to connect as
      do
         !!username.copy(to);
      end -- set_username

   set_password(to: STRING) is
      -- Set password for authentication
      do
         !!password.copy(to);
      end -- set_password


feature {ANY} -- status

	is_connected: BOOLEAN is
		-- Find out if we're connected.
		do
         Result := (conn /= Void);
		end

	has_results: BOOLEAN is
		-- Find out if we have results
		do
         Result := (res /= Void);
		end

feature {PG}
   -- Internal data stuff

   conn: POINTER;
      -- Connection holder for C library.

   res: POINTER;
      -- Result holder for C library.

   host: STRING;
      -- Database host

   port: STRING;
      -- Database port

   options: STRING;
      -- Database options

   tty: STRING;
      -- Database tty

   dbname: STRING;
      -- Database name

   username: STRING;
      -- Database username

   password: STRING;
      -- Database password

feature {PG}

   pg_connect(h, p, o, t, d, u, pass: POINTER): POINTER is
      external "C_WithoutCurrent"
      end -- pg_connect

   pg_query(c, q: POINTER): POINTER is
      external "C_WithoutCurrent"
      end -- pg_query

   pg_intersect(r: POINTER; i, j: INTEGER): POINTER is
      external "C_WithoutCurrent"
      alias "PQgetvalue"
      end -- pg_intersect

   pg_ntuples(r: POINTER): INTEGER is
      external "C_WithoutCurrent"
      alias "PQntuples"
      end -- pg_ntuples

   pg_nfields(r: POINTER): INTEGER is
      external "C_WithoutCurrent"
      alias "PQnfields"
      end -- pg_nfields

   pg_finish(r: POINTER) is
      external "C_WithoutCurrent"
      alias "PQfinish"
      end -- pg_finish

end -- class PG
