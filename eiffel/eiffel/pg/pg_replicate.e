indexing
   description: "Postgres database replication...";
   version: "$Revision: 1.7 $";
   author: "Dustin Sallings <dustin@spy.net>";
   copyright: "1999";
   license: "See forum.txt.";

class PG_REPLICATE
-- The replicator

creation {ANY}
   make

feature {NONE}
   -- Make is private

   make(s, d: PG) is
      -- Make a PG_REPLICATE object from s to d
      require
         s.is_connected;
         d.is_connected;
      do
         db_from := s;
         db_to := d;
      end -- make

feature {ANY} -- Replication services

   rep_seq(seq: STRING) is
      -- Replicate a sequence
      require
         seq /= Void;
      local
         query: STRING;
         a: ARRAY[STRING];
      do
         query := "select last_value from " + seq;
         db_from.query(query);
         check
            db_from.get_row;
         end;
         a := db_from.last_row;
         query := "select setval(" + db_from.quote(seq) + ", " + a @ 0 + ")";
         debug
            io.put_string("Setting " + seq + " to " + a @ 0 + ".%N");
         end;
         db_to.query(query);
      end -- rep_seq

   rep_table(table: STRING) is
      -- Replicate a table
      require
         table /= Void;
      local
         b: BOOLEAN;
         query: STRING;
         retry_attempt: INTEGER;
      do
         debug
            io.put_string("Replicating ");
            io.put_string(table);
            io.put_string(".%N");
            io.put_string("Beginning transaction on the to thing.%N");
         end;
         db_from.copy_from(table);
         db_to.begin;
         db_to.query("delete from " + table);
         db_to.copy_to(table);
         from
            b := db_from.getline;
         until
            b = false
         loop
            db_to.putline(db_from.last_line + "%N");
            b := db_from.getline;
         end;
         db_to.endcopy;
         db_from.endcopy;
         db_to.commit;
      rescue
         if retry_attempt < max_retry_attempts then
            retry_attempt := retry_attempt + 1;
            debug
               io.put_string("PG_REPLICATE: Retrying...%N");
            end;
            retry;
         end;
      end -- rep_table

   full is
      -- Full replication of everything we know about.
      local
         a: ARRAY[STRING];
         i: INTEGER;
      do
         unindex;
         a := db_from.tables;
         from
            i := a.lower;
         until
            i > a.upper
         loop
            rep_table(a @ i);
            i := i + 1;
         end;
         a := db_from.sequences;
         from
            i := a.lower;
         until
            i > a.upper
         loop
            rep_seq(a @ i);
            i := i + 1;
         end;
         reindex;
      end -- full

feature {NONE}
   -- Private stuff

   unindex is
      local
         a, list: ARRAY[STRING];
         i: INTEGER;
         q: STRING;
         b: BOOLEAN;
      do
         debug
            io.put_string("Dropping all indices%N");
         end;
         !!q.copy("select * from pg_indexes where tablename not like 'pg_%%'");
         db_to.query(q);
         from
            b := db_to.get_row;
            !!list.with_capacity(16,16);
            list.clear;
         until
            b = false
         loop
            a := db_to.last_row;
            list.add_last(a @ 1);
            b := db_to.get_row;
         end;
         from
            i := list.lower;
         until
            i > list.upper
         loop
            q := "drop index " + list @ i;
            db_to.query(q);
            i := i + 1;
         end;
      end -- unindex

   reindex is
      local
         a: ARRAY[STRING];
         q: STRING;
         b: BOOLEAN;
      do
         debug
            io.put_string("Recreating all indices%N");
         end;
         !!q.copy("select * from pg_indexes where tablename not like 'pg_%%'");
         db_from.query(q);
         from
            b := db_from.get_row;
         until
            b = false
         loop
            a := db_from.last_row;
            db_to.query(a @ 2);
            b := db_from.get_row;
         end;
      end -- reindex

   join(with: STRING; a: ARRAY[STRING]): STRING is
      -- Join an array of strings with the given string
      require
         a /= Void;
         a.count > 0;
         with /= Void;
      local
         i: INTEGER;
      do
         !!Result.make(256);
         from
            i := a.lower;
         until
            i >= a.upper
         loop
            -- Result.append(db_to.quote(a @ i));
            Result.append(a @ i);
            Result.append(with);
            i := i + 1;
         end;
         Result.append(a @ a.upper);
      end -- join

   max_retry_attempts: INTEGER is 3;

   db_from: PG;

   db_to: PG;

end -- class PG_REPLICATE
