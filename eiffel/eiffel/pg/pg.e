--
-- Copyright (c) 1999  Dustin Sallings
--
-- $Id: pg.e,v 1.1 1999/05/19 07:36:19 dustin Exp $
--

indexing
	description: "Postgres database access...";

class PG

creation {ANY}
	make

feature {ANY}

	conn: POINTER;

	res: POINTER;

	current_row: INTEGER;

	connect(host, db: STRING): BOOLEAN is
		-- Make a database connection
		do
			conn:=pg_connect(host.to_external, db.to_external);
			if conn = Void then
				Result:=false;
			else
				Result:=true;
			end
		end -- connect

	query(q: STRING): BOOLEAN is
		-- Query on an open database connection
		require
			connected: conn /= Void
		do
			res:=pg_query(conn, q.to_external);
			if res = Void then
				Result:=false;
			else
				Result:=true;
			end
		end -- query

	get_row is
		require
			not_results: res /= Void
		local
			i, fields: INTEGER;
			s: STRING;
			p: POINTER;
		do
			from
				fields:=pg_nfields(res);
				i:=0
			until
				i>=fields
			loop
				p:=pg_intersect(res, current_row, i);
				!!s.from_external(p);
				io.put_string(s);
				io.put_string("%T");
				i:=i+1;
			end
			io.put_string("%N");
			current_row:=current_row+1;
		end -- get_row

	make is
		local
			b: BOOLEAN;
		do
			if (not connect("bleu", "machine")) then
				io.put_string("NOT Connected%N");
			end

			if query("select * from oems order by name;") then
				from
					current_row:=0
				until
					current_row>=pg_ntuples(res)
				loop
					get_row
				end
			end
		end

feature {NONE}

	pg_connect(host, database: POINTER): POINTER is
	external "C"
	end;

	pg_query(c, q: POINTER): POINTER is
	external "C"
	end;

	pg_intersect(r: POINTER; i, j: INTEGER): POINTER is
	external "C" alias "PQgetvalue"
	end;

	pg_ntuples(r: POINTER): INTEGER is
	external "C" alias "PQntuples"
	end;

	pg_nfields(r: POINTER): INTEGER is
	external "C" alias "PQnfields"
	end;

end
