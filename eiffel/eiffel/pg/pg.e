indexing
   description: "Postgres database access...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pg.e,v 1.3 1999/05/25 06:23:40 dustin Exp $
--
class PG

creation {ANY}
   execute, make

feature {ANY}

   conn: POINTER;

   res: POINTER;

   current_row: INTEGER;

   last_row: ARRAY[STRING];

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

   execute is
      local
         b: BOOLEAN;
         a: ARRAY[STRING];
         i: INTEGER;
      do
         if not connect("bleu","machine") then
            io.put_string("NOT Connected%N");
         end;
         if query("select * from oems order by name;") then
            from
               current_row := 0;
            until
               current_row >= pg_ntuples(res)
            loop
               if get_row then
                  a := last_row;
               end;
               from
                  i := 0;
               until
                  i >= a.count
               loop
                  io.put_string(a @ i);
                  io.put_string("%T");
                  i := i + 1;
               end;
               io.put_string("%N");
            end;
         end;
      end -- execute

   make is
      do
		execute;
      end -- make

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

end -- class PG
