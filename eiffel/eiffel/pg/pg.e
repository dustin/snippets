indexing
   description: "Postgres database access...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pg.e,v 1.4 1999/05/25 06:45:12 dustin Exp $
--
class PG

creation {ANY}
   make

feature {ANY}

   current_row: INTEGER;
      -- Current row number we're on.

   last_row: ARRAY[STRING];
      -- Last row retrieved.

   connect(host, db: STRING): BOOLEAN is
      -- Make a database connection
      do
         conn := pg_connect(host.to_external,db.to_external);
         if conn = Void then
            Result := false;
         else
            Result := true;
         end;
      end -- connect

   query(q: STRING): BOOLEAN is
      -- Query on an open database connection
      require
         connected: conn /= Void;
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
         no_results: res /= Void;
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

   make is
      -- Doesn't really do anything, but we need a make.
      do
         current_row := 0;
      end -- make

feature {NONE}
   -- Internal data stuff
   -- Connection holder for C library.

   conn: POINTER;
      -- Result holder for C library.

   res: POINTER;

feature {NONE}

   pg_connect(host, database: POINTER): POINTER is
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
