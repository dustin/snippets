class
	LOOKUP

creation
	make

feature {NONE} -- Just a program, nothing public

	search_base: STRING is "dc=spy,dc=net";

	ldap: LDAP;

	display_attribute(attr: STRING) is
		local
			a: ARRAY[STRING];
			i: INTEGER;
			tried: BOOLEAN;
		do
			if not tried then
				-- One shot at this, if it fails, we don't care.
				tried:=true;
				a:=ldap.get_values(attr);
				io.put_string(attr + ":%N");
				from
					i:=a.lower;
				until
					i>a.upper
				loop
					io.put_string("%T" + a @ i + "%N");
					i:=i+1;
				end
			end
		rescue
			retry;
		end

	do_search is
		local
			s: STRING;
			tried, errored: BOOLEAN;
		do
			if not tried then
				tried:=true;
				!!s.copy("uid=" + argument(1));
				io.put_string("Searching for " + s + "%N");
				ldap.search(s, 2);
				io.put_string("Search finished, found ");
				io.put_integer(ldap.nresults);
				io.put_string(" matches.%N");
				ldap.first_entry;

				display_attribute("cn");
				display_attribute("mail");
				display_attribute("telephonenumber");
			else
				if not errored then
					errored:=true;
					io.put_string(ldap.error_message + "%N");
				end
			end
		rescue
			io.put_string("Retrying search...%N");
			retry
		end

	make is
		do
			!!ldap;
			-- ldap.set_searchbase(search_base);
			ldap.connect;
			do_search;
		end
end
