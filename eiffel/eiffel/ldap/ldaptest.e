class
	LDAPTEST

creation
	make

feature {NONE} -- make and data

	ldap: LDAP;

	make is
		do
			!!ldap;
			ldap.set_binddn("uid=dustin,ou=Agents,dc=spy,dc=net");
			io.put_string("Enter password:  ");
			io.read_line;
			ldap.set_bindpw(io.last_string);

			ldap.connect;
			ldap.bind;
			io.put_string("Correct!%N");
		rescue
			io.put_string("Incorrect password%N");
			retry;
		end

end
