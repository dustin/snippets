class
	LDAPTEST

creation
	make

feature {NONE} -- make and data

	ldap: LDAP;

	make is
		local
			bound: BOOLEAN;
			retries: INTEGER;
			a, b: ARRAY[STRING];
			i, j: INTEGER;
		do
			if not bound then
				!!ldap;
				ldap.set_binddn("uid=dustin,ou=Agents,dc=spy,dc=net");
				io.put_string("Enter password:  ");
				io.read_line;
				ldap.set_bindpw(io.last_string);

				ldap.connect;
				ldap.bind;
				bound:=true;
				io.put_string("Correct!%N");
			end

			ldap.set_searchbase("dc=spy,dc=net");

			io.put_string("Doing search.%N");
			ldap.search("uid=sidney", 2);
			io.put_string("Search was successful, found ");
			io.put_integer(ldap.nresults);
			io.put_string(" matches.%NAttributes:%N");

			ldap.first_entry;

			from
				a:=ldap.list_attributes;
				i:=a.lower;
			until
				i > a.upper
			loop
				io.put_string("%T" + a @ i + " (");
				b:=ldap.get_values(a @ i);
				io.put_integer(b.count);
				io.put_string(" match(es))%N");
				from
					j:=b.lower;
				until
					j>b.upper
				loop
					io.put_string("%T%T" + b @ j + "%N");
					j:=j+1;
				end
				i:=i+1;
			end

		rescue
			if not bound then
				io.put_string("Incorrect password%N");
			else
				io.put_string("Search failed%N");
			end
			if retries < 2 then
				retries:=retries+1;
				retry;
			end
		end

end
