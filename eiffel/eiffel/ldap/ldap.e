class
	LDAP

feature {NONE} -- make and data

	ldap_handle: POINTER;

	ldap_host: STRING;

	ldap_port: INTEGER;

	ldap_binddn: STRING;

	ldap_bindpw: STRING;

feature {ANY} -- Initialization stuff

	set_host(to: STRING) is
		-- Set the server to bind to
		do
			ldap_host:=to;
		end

	set_port(to: INTEGER) is
		-- Set the port to bind to
		do
			ldap_port:=to;
		end

	set_binddn(to: STRING) is
		-- Set the DN to bind as
		do
			ldap_binddn:=to;
		end

	set_bindpw(to: STRING) is
		-- Set the bind password
		do
			ldap_bindpw:=to;
		end

	set_searchbase(to: STRING) is
		-- Set the search base
		do
			c_ldap_set_sb(ldap_handle, to.to_external);
		end

	connect is
		-- Connect to an LDAP server
		local
			host: POINTER;
		do
			if ldap_host /= Void then
				host:=ldap_host.to_external;
			end
			ldap_handle:=c_ldap_init(host, ldap_port);
		ensure
			ldap_handle.is_not_null;
		end

	bind is
		-- Bind to an LDAP server
		local
			binddn, bindpw: POINTER;
		do
			if ldap_binddn /= Void then
				binddn:=ldap_binddn.to_external;
			end
			if ldap_bindpw /= Void then
				bindpw:=ldap_bindpw.to_external;
			end
			check
				c_ldap_bind(ldap_handle, binddn, bindpw);
			end;
		end

	search(filter: STRING; scope: INTEGER) is
		-- Execute a search
		do
			check
				c_ldap_search(ldap_handle, filter.to_external, scope);
			end
		end

	nresults: INTEGER is
		-- Find out how many entries we found
		do
			Result:=c_ldap_nresults(ldap_handle);
		end

	first_entry: BOOLEAN is
		-- Get the first entry
		do
			Result:=c_ldap_first_entry(ldap_handle);
		end

	dispose is
		-- go away
		do
			c_ldap_destroy(ldap_handle);
		end

feature {NONE} -- C functions

	c_ldap_init(host: POINTER; port: INTEGER): POINTER is
		-- The init
		external "C"
		end

	c_ldap_set_sb(ld, search_base: POINTER) is
		-- set the search base
		external "C"
		end

	c_ldap_set_timeout(ld: POINTER; timeout: INTEGER) is
		-- set the search timeout
		external "C"
		end

	c_ldap_bind(ld, binddn, bindpw: POINTER): BOOLEAN is
		-- Bind
		external "C"
		end

	c_ldap_search(ld, filter: POINTER; scope: INTEGER): BOOLEAN is
		-- Search
		external "C"
		end

	c_ldap_destroy(ld: POINTER) is
		-- Free up everything
		external "C"
		end

	c_ldap_nresults(ld: POINTER): INTEGER is
		-- Find out how many results there were from the last search
		external "C"
		end

	c_ldap_first_entry(ld: POINTER): BOOLEAN is
		-- Get the first entry
		external "C"
		end

end
