indexing
   description: "The Replicator...";
   version: "$Revision: 1.1 $";
class PG_REPLICATE

creation {ANY}
   make

feature {ANY}

   make(s, d: PG) is
      do
         set_db_from(s);
         set_db_to(d);
      end -- make

feature {ANY} -- Replication services

   set_db_from(to: PG) is
      -- Source database thingy
      do
         db_from := to;
      end -- set_db_from

   set_db_to(to: PG) is
      -- Destination database thingy
      do
         db_to := to;
      end -- set_db_to

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
         query := "select setval(" + seq + "(" + a @ 0 + ")";
         io.put_string("Setting " + seq + " to " + a @ 0 + ".%N");
         db_to.query(query);
      end -- rep_seq

   rep_table(table: STRING) is
      -- Replicate a table
      local
         a: ARRAY[STRING];
         b: BOOLEAN;
         query: STRING;
         retry_attempt: INTEGER;
      do
         io.put_string("Replicating ");
         io.put_string(table);
         io.put_string(".%N");
         db_to.begin;
         query := "delete from " + table;
         db_to.query(query);
         query := "select * from " + table;
         db_from.query(query);
         from
            b := db_from.get_row;
         until
            b = false
         loop
            a := db_from.last_row;
            query := "insert into " + table + " values(" + join(", ",a) + ")";
            db_to.query(query);
            b := db_from.get_row;
         end;
         db_to.commit;
      rescue
         if retry_attempt < max_retry_attempts then
            retry_attempt := retry_attempt + 1;
            retry;
         end;
      end -- rep_table

   full is
      local
         a: ARRAY[STRING];
         i: INTEGER;
         query: STRING;
         retry_attempt: INTEGER;
      do
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
      end -- full

feature {NONE}
   -- Private stuff

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
            Result.append(db_to.quote(a @ i));
            Result.append(with);
            i := i + 1;
         end;
         Result.append(db_to.quote(a @ a.upper));
      end -- join

   max_retry_attempts: INTEGER is 3;

   db_from: PG;

   db_to: PG;

end -- class PG_REPLICATE
