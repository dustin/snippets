indexing
	description: "Todo!";
	revision: "$Revision: 1.4 $";

class TODO

creation {ANY} make

feature

	make is
		do
			io.put_string("Content-type: text/html%N%N");
			connect_to_db;

			io.put_string("<h2>Unfinished only</h2>");
			unset_limiters;
			set_current(true);
			show_all;

			io.put_string("<h2>Finished only</h2>");
			unset_limiters;
			set_finished(true);
			show_all;

			unset_limiters;
			io.put_string("<h2>Dustin</h2>");
			set_username("dustin");
			show_all;

			unset_limiters;
			io.put_string("<h2>Jason</h2>");
			set_username("thanatos");
			show_all;

		end

feature {ANY} -- Looking stuff up

	entries: ARRAY[TODO_ENTRY] is
		-- Get matching entries.
		local
			query, w, tmp: STRING;
			b: BOOLEAN;
			e: TODO_ENTRY;
		do
			connect_to_db;

			!!w.copy("where"); -- We change this when we need and.

			!!query.copy("select * from task_view ");

			if p_username /= Void then
				-- Only get entries for one user.
				tmp:=db.quote(p_username);
				query.append(w);
				query.append(" user_name=");
				query.append(tmp);
				query.append("%N");
				!!w.copy("and");
			end

			if p_current_only then
				-- Only get current entries.
				query.append(w);
				query.append(" finished is null%N");
				!!w.copy("and");
			end

			if p_finished_only then
				-- Only get finished entries.
				query.append(w);
				query.append(" finished is not null%N");
				!!w.copy("and");
			end

			if p_latest /= Void then
				-- Don't get items due before this date
				tmp:=db.quote(p_latest);
				query.append(w);
				query.append(" latest < ");
				query.append(tmp);
				query.append("%N");
				!!w.copy("and");
			end

			if p_sortby /= Void then
				query.append("order by ")
				query.append(p_sortby);
			else
				query.append("order by priority, latest%N")
			end

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

feature {ANY} -- Entry limiters

	set_username(s: STRING) is
		-- Only look up this user's todo list
		do
			p_username:=s;
		end;

	set_current(b: BOOLEAN) is
		-- Do we only want current entries (i.e. not finished)?
		do
			p_current_only:=b;
		end

	set_finished(b: BOOLEAN) is
		-- Do we only want finished entries?
		do
			p_finished_only:=b;
		end

	set_latest(s: STRING) is
		-- Don't get entries due before this date.
		do
			p_latest:=s;
		end

	set_sortby(s: STRING) is
		-- Don't get entries due before this date.
		do
			p_sortby:=s;
		end

	unset_limiters is
		-- Unset all restrictions, get everything.
		do
			p_username:=Void;
			p_current_only:=false;
			p_finished_only:=false;
			p_latest:=Void;
			p_sortby:=Void;
		end;

feature {TODO} -- private stuff for querying

	p_username: STRING;

	p_current_only: BOOLEAN;

	p_finished_only: BOOLEAN;

	p_latest: STRING;

	p_sortby: STRING;

feature {ANY} -- Status

	is_connected: BOOLEAN is
		-- Are we connected to the database?
		do
			Result:= ( db.is_connected );
		end

feature {ANY} -- Misc debug type stuff

	show_all is
		-- Dump all matching entries in HTML.
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
			show_th("Description");
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
			db.is_connected
		end

	db: PG;
		-- The actual database connection

end
