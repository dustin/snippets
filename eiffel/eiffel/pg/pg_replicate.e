indexing
   description: "Postgres database replication...";
   version: "$Revision: 1.5 $";
   author: "Dustin Sallings <dustin@spy.net>";
   copyright: "1999";
   license: "See forum.txt.";
class PG_REPLICATE
-- The replicator

creation {ANY}
   make

feature {NONE}  -- Make is private

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
         io.put_string("Setting " + seq + " to " + a @ 0 + ".%N");
         db_to.query(query);
      end -- rep_seq

   rep_table(table: STRING) is
      -- Replicate a table
	  require
		table /= Void;
      local
         a: ARRAY[STRING];
         b: BOOLEAN;
         query: STRING;
         retry_attempt: INTEGER;
      do
         io.put_string("Replicating ");
         io.put_string(table);
         io.put_string(".%N");
         debug
            io.put_string("Beginning transaction on the to thing.%N");
         end;
         db_to.begin;
         query := "delete from " + table;
         db_to.query(query);
         debug
            io.put_string("Beginning transaction on the from thing.%N");
         end;
         db_from.begin;
         query := "declare c cursor for select * from " + table;
         db_from.query(query);
         from
            db_from.query("fetch 1000 in c");
         until
            db_from.num_rows < 1
         loop
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
            db_from.query("fetch 1000 in c");
         end;
         db_to.commit;
         db_from.query("end");
      rescue
         if retry_attempt < max_retry_attempts then
            retry_attempt := retry_attempt + 1;
            retry;
         end;
      end -- rep_table

   full is
	-- Full replication of everything we know about.
      local
         a: ARRAY[STRING];
         i: INTEGER;
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
