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
			rep_oem;
			rep_strings;
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
				db_to.query(query);
			end
		end

	rep_oem is
		local
			a: ARRAY[STRING];
			b: BOOLEAN;
			query: STRING;
		do
			db_to.query("delete from oem");
			db_from.query("select * from oem");
			from
				b:=db_from.get_row;
			until
				b=false
			loop
				a:=db_from.last_row;
				b:=db_from.get_row;
				!!query.copy("insert into oem values(");
				query.append(a @ 0);
				query.append(",");
				query.append(db_to.quote(a @ 1));
				query.append(")%N");
				io.put_string(query);
				db_to.query(query);
			end
		end

	rep_strings is
		local
			a: ARRAY[STRING];
			b: BOOLEAN;
			query: STRING;
		do
			db_to.query("delete from strings");
			db_from.query("select * from strings");
			from
				b:=db_from.get_row;
			until
				b=false
			loop
				a:=db_from.last_row;
				b:=db_from.get_row;
				!!query.copy("insert into strings values(");
				query.append(a @ 0);
				query.append(",");
				query.append(db_to.quote(a @ 1));
				query.append(",");
				query.append(db_to.quote(a @ 2));
				query.append(")%N");
				io.put_string(query);
				db_to.query(query);
			end
		end

	db_from: PG;

	db_to: PG;
end
