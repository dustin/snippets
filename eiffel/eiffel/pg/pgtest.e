indexing
   description: "Postgres database test...";
--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pgtest.e,v 1.3 1999/06/02 01:33:58 dustin Exp $
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
         db.set_dbname("machine");
         db.connect;
         db.query("select * from oems order by name;");
         from
            b := db.get_row;
         until
            b = false
         loop
            a := db.last_row;
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
            b := db.get_row;
         end;
      end -- make

end -- class PGTEST
