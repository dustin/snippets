indexing
   description: "Postgres database test...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pgtest.e,v 1.6 2002/11/23 08:31:38 dustin Exp $
--
class PGTEST

creation {ANY}
   make

feature {ANY}

   make is
      local
         a: ARRAY[STRING];
         i: INTEGER;
         b: BOOLEAN;
         db: PG;
      do
         !!db.make;
         db.set_dbname("dustin");
         db.set_username("dustin");
         db.set_password("blahblah");
         db.set_host("db");
         db.connect;
         a := db.tables;
         io.put_string("Tables:%N");
         from
            i := a.lower;
         until
            i > a.upper
         loop
            io.put_string("%T" + a @ i + "%N");
            i := i + 1;
         end;
         a := db.sequences;
         io.put_string("Sequences:%N");
         from
            i := a.lower;
         until
            i > a.upper
         loop
            io.put_string("%T" + a @ i + "%N");
            i := i + 1;
         end;
         io.put_string("Query:%N> ");
         io.read_line;
         io.put_string(io.last_string);
         io.put_string("%N");
         db.query(io.last_string);
         from
            b := db.get_row;
         until
            b = false
         loop
            a := db.last_row;
            from
               i := a.lower;
            until
               i > a.upper
            loop
               io.put_string(a @ i + "%T");
               i := i + 1;
            end;
            io.put_string("%N");
            b := db.get_row;
         end;
      end -- make

end -- class PGTEST
