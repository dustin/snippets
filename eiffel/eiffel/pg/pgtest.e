indexing
   description: "Postgres database test...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pgtest.e,v 1.4 1999/06/03 18:06:38 dustin Exp $
--
class PGTEST

creation {ANY}
   make

feature {ANY}

   make is
      local
         b: BOOLEAN;
         a: ARRAY[STRING];
         i: INTEGER;
         db: PG;
      do
         !!db.make;
         db.set_dbname("events");
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

      end -- make

end -- class PGTEST
