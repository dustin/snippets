indexing
	description: "Todo!";
	revision: "$Revision: 1.1 $";

class TODO creation
	make

feature

	make is
		do
			io.put_string("Content-type: text/html%N%N");
			connect_to_db;
			show_all;
		end

	show_all is
		local
			a: ARRAY[STRING];
			b: BOOLEAN;
			i: INTEGER;
		do
			connect_to_db;
			if db.query("select * from task_view") then
				io.put_string("<html><head><title>Todo Tasks</title>%
				%<body bgcolor=%"fFfFfF%">%N<h2>Todo Tasks</h2>%N");
				io.put_string("<table border=%"1%">");
				from
					b:=db.get_row;
				until
					b=false
				loop
					a:=db.last_row;

					io.put_string("<tr>");

					from
						i:=1;
					until
						i>=a.count
					loop
						io.put_string("<td>");
						io.put_string(a.item(i));
						io.put_string("</td>");
						i:=i+1;
					end
					io.put_string("</tr>%N");

					b:=db.get_row;
				end
				io.put_string("</table></body></html>%N");
			else -- query failed
				io.put_string("Query failed...%N");
			end;
		end

feature {NONE}

	connect_to_db is
		local
			b: BOOLEAN;
		once
			!!db.make;
			db.set_host("bleu");
			db.set_dbname("todo");
			db.set_username("dustin");
			b:=db.connect;
		ensure
			b = true;
		end

	db: PG;

end
