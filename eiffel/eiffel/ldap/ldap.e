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
		do
			ldap_host:=to;
		end

	set_port(to: INTEGER) is
		do
			ldap_port:=to;
		end

	set_binddn(to: STRING) is
		do
			ldap_binddn:=to;
		end

	set_bindpw(to: STRING) is
		do
			ldap_bindpw:=to;
		end

	connect is
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

feature {NONE} -- C functions

	c_ldap_init(host: POINTER; port: INTEGER): POINTER is
		external "C"
		end

	c_ldap_bind(ld, binddn, bindpw: POINTER): BOOLEAN is
		external "C"
		end
end
