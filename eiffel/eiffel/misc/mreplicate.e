class MREPLICATE

creation make

feature

	make is
		do
			!!db_from.make;
			db_from.set_dbname("modems");
			db_from.set_host("propaganda.spy.net");
			db_from.connect;

			!!db_to.make;
			db_to.set_dbname("modems");
			db_to.connect;

			rep_mod;
			-- rep_oem;
			-- rep_strings;
		end

feature {NONE}

	rep_mod is
		local
			a: ARRAY[STRING];
			b: BOOLEAN;
			query: STRING;
		do
			db_to.query("delete from mod");
			db_from.query("select * from mod");
			from
				b:=db_from.get_row;
			until
				b=false
			loop
				a:=db_from.last_row;
				b:=db_from.get_row;
				!!query.copy("insert into mod values(");
				query.append(a @ 0);
				query.append(",");
				query.append(a @ 1);
				query.append(",");
				query.append(db_to.quote(a @ 2));
				query.append(")%N");

				io.put_string(query);
			end
		end

	db_from: PG;

	db_to: PG;
end
