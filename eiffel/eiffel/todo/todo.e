indexing
	description: "Todo!";
	revision: "$Revision: 1.2 $";

class TODO

creation {ANY} make

feature

	make is
		do
			io.put_string("Content-type: text/html%N%N");
			connect_to_db;
			show_all;
		end

feature {ANY} -- Looking stuff up

	entries: ARRAY[TODO_ENTRY] is
		local
			query, w: STRING;
			b: BOOLEAN;
			e: TODO_ENTRY;
		do
			connect_to_db;

			!!w.copy("where"); -- We change this when we need and.

			!!query.copy("select * from task_view ");

			if db.query(query) then
				!!Result.make(0, 0);
				Result.clear;

				from
					b:=db.get_row;
				until
					b=false
				loop

					!!e.from_array(db.last_row);
					Result.add_last(e);

					b:=db.get_row; -- get the next row
				end -- data loop
			end -- Query worked
		end

feature {ANY} -- Status

	is_connected: BOOLEAN is
		-- Are we connected to the database?
		do
			Result:= ( db.is_connected );
		end

feature {ANY} -- Misc debug type stuff

	show_all is
		-- Dump it all in HTML.
		local
			a: ARRAY[TODO_ENTRY];
			i: INTEGER;
			e: TODO_ENTRY;
		do
			a:=entries;

			io.put_string("<html><head><title>Todo Tasks</title>");
			io.put_string("<body bgcolor=%"fFfFfF%">%N<h2>Todo Tasks</h2>%N");
			io.put_string("<table border=%"1%">");

			show_th("Username");
			show_th("Task ID");
			show_th("User ID");
			show_th("Earliest");
			show_th("Latest");
			show_th("Priority");
			show_th("Summary");
			show_td("Description");
			show_th("Finished");
			show_th("Created");

			from
				i:=a.lower;
			until
				i>a.upper
			loop

				e:=a.item(i); -- get an entry

				io.put_string("<tr>");

				show_td(e.user_name);
				show_td(e.task_id);
				show_td(e.user_id);
				show_td(e.earliest);
				show_td(e.latest);
				show_td(e.priority);
				show_td(e.summary);
				show_td(e.description);
				show_td(e.finished);
				show_td(e.created);

				io.put_string("</tr>%N");
				i:=i+1;
			end

			io.put_string("</table></body></html>%N");
		end

feature {NONE}

	show_td(s: STRING) is
		require
			s /= Void;
		do
			io.put_string("<td>");
			io.put_string(s);
			io.put_string("</td>");
		end

	show_th(s: STRING) is
		require
			s /= Void;
		do
			io.put_string("<th>");
			io.put_string(s);
			io.put_string("</th>");
		end

	connect_to_db is
		-- Get a database connection.
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
		-- The actual database connection

end
