indexing
   author: "Dustin Sallings <dustin@spy.net>";
copyright: "1997 Dustin Sallings <dustin@spy.net>";
license: "See forum.txt";
version: "$Revision: 1.9 $";
class LDAP
-- LDAP Access Routines.

inherit 
   MEMORY
      redefine dispose
      end; 
   
feature {NONE} 
   -- data and destruction

   ldap_handle: POINTER;
   
   ldap_host: STRING;
   
   ldap_port: INTEGER;
   
   ldap_binddn: STRING;
   
   ldap_bindpw: STRING;
   
   ldap_got_entry: BOOLEAN;
   
   ldap_got_search: BOOLEAN;
   
   ldap_have_mods: BOOLEAN;
   
   dispose is 
      -- go away
      do  
         c_ldap_destroy(ldap_handle);
      end -- dispose

feature {ANY} -- Initialization stuff

   set_host(to: STRING) is 
      -- Set the server to bind to
      do  
         ldap_host := to;
      end -- set_host
   
   set_port(to: INTEGER) is 
      -- Set the port to bind to
      do  
         ldap_port := to;
      end -- set_port
   
   set_binddn(to: STRING) is 
      -- Set the DN to bind as
      do  
         ldap_binddn := to;
      end -- set_binddn
   
   set_bindpw(to: STRING) is 
      -- Set the bind password
      do  
         ldap_bindpw := to;
      end -- set_bindpw
   
   set_searchbase(to: STRING) is 
      -- Set the search base
      do  
         c_ldap_set_sb(ldap_handle,to.to_external);
      end -- set_searchbase
   
   connect is 
      -- Connect to an LDAP server
      local 
         host: POINTER;
      do  
         if ldap_host /= Void then 
            host := ldap_host.to_external;
         end; 
         ldap_handle := c_ldap_init(host,ldap_port);
      ensure 
         connected; 
      end -- connect
   
   bind is 
      -- Bind to an LDAP server, otherwise, we're anonymous
      require 
         connected; 
      local 
         binddn, bindpw: POINTER;
      do  
         if ldap_binddn /= Void then 
            binddn := ldap_binddn.to_external;
         end; 
         if ldap_bindpw /= Void then 
            bindpw := ldap_bindpw.to_external;
         end; 
         check 
            c_ldap_bind(ldap_handle,binddn,bindpw); 
         end;
      end -- bind

feature {ANY} -- Searching

   search(filter: STRING; scope: INTEGER) is 
      -- Execute a search on a filter
      require 
         connected; 
      do  
         ldap_got_entry := false;
         ldap_got_search := c_ldap_search(ldap_handle,filter.to_external,scope);
      ensure 
         got_search; 
      end -- search
   
   nresults: INTEGER is 
      -- Find out how many entries we found
      require 
         got_search; 
      do  
         Result := c_ldap_nresults(ldap_handle);
      end -- nresults
   
   first_entry is 
      -- Get the first entry
      require 
         got_search; 
      do  
         ldap_got_entry := c_ldap_first_entry(ldap_handle);
      end -- first_entry
   
   next_entry is 
      -- Get the next entry
      require 
         got_search; 
      do  
         ldap_got_entry := c_ldap_next_entry(ldap_handle);
      end -- next_entry
   
   list_attributes: ARRAY[STRING] is 
      -- List all attributes in the current entry.
      -- NOTE: You must call first_entry before you call this.
      require 
         got_entry; 
      local 
         s: STRING;
         p: POINTER;
      do  
         !!Result.with_capacity(16,16);
         Result.clear;
         from 
            p := c_ldap_first_attribute(ldap_handle);
         until 
            p.is_null
         loop 
            !!s.from_external_copy(p);
            Result.add_last(s);
            p := c_ldap_next_attribute(ldap_handle);
         end; 
      end -- list_attributes
   
   get_values(att: STRING): ARRAY[STRING] is 
      -- Get the values for a given attribute in an existing entry.
      -- NOTE: You must call first_entry before you call this.
      require 
         got_entry; 
      local 
         s: STRING;
         p: POINTER;
         i: INTEGER;
      do  
         !!Result.with_capacity(16,16);
         Result.clear;
         from 
            i := 0;
            p := c_ldap_get_value(ldap_handle,att.to_external,i);
         until 
            p.is_null
         loop 
            !!s.from_external_copy(p);
            Result.add_last(s);
            i := i + 1;
            p := c_ldap_get_value(ldap_handle,att.to_external,i);
         end; 
      end -- get_values

feature {ANY} -- Compare

   compare(dn, attr, value: STRING): BOOLEAN is 
      -- Do an LDAP comparison to see if there's an attr=value for the
      -- specified dn.
      require 
         dn /= Void; 
         attr /= Void; 
         value /= Void; 
      do  
         Result := c_ldap_compare(ldap_handle,dn.to_external,attr.to_external,value.to_external);
      end -- compare

feature {ANY} -- Add/modify

   mod_add(attr, value: STRING) is 
      -- Store an entry to be added in a modify or add
      require 
         attr /= Void; 
         value /= Void; 
         connected; 
      do  
         c_ldap_mod_add(ldap_handle,attr.to_external,value.to_external,value.count);
         ldap_have_mods := true;
      end -- mod_add
   
   mod_replace(attr, value: STRING) is 
      -- Store an entry to be replaced in a modify
      require 
         attr /= Void; 
         value /= Void; 
         connected; 
      do  
         c_ldap_mod_replace(ldap_handle,attr.to_external,value.to_external,value.count);
         ldap_have_mods := true;
      end -- mod_replace
   
   mod_delete(attr: STRING) is 
      -- Store an entry to be deleted in a modify
      require 
         attr /= Void; 
         connected; 
      do  
         c_ldap_mod_delete(ldap_handle,attr.to_external);
         ldap_have_mods := true;
      end -- mod_delete
   
   add(dn: STRING) is 
      -- Add an LDAP entry for the given DN and predefined set of mods
      require 
         connected; 
         dn /= Void; 
         have_mods; 
      do  
         check 
            c_ldap_add(ldap_handle,dn.to_external); 
         end;
         mod_clean;
      rescue 
         -- For rescue, we're just going to run the mod_clean
         -- and let the assertion ride back.
         mod_clean;
      end -- add
   
   modify(dn: STRING) is 
      -- Modify an LDAP entry for the given DN and predefined set of mods
      require 
         connected; 
         dn /= Void; 
         have_mods; 
      do  
         check 
            c_ldap_modify(ldap_handle,dn.to_external); 
         end;
         mod_clean;
      rescue 
         -- For rescue, we're just going to run the mod_clean
         -- and let the assertion ride back.
         mod_clean;
      end -- modify
   
   mod_clean is 
      -- Clean up mod list.
      -- There is a bug in here triggered by doing an attribute delete,
      -- Eiffel's nice enough to cover it up until I can deal with it.
      local 
         done: BOOLEAN;
      do  
         if not done then 
            done := true;
            c_ldap_mod_clean(ldap_handle);
            ldap_have_mods := false;
         end; 
      rescue 
         retry;
      end -- mod_clean

feature {ANY} -- Delete

   delete(dn: STRING) is 
      require 
         connected; 
         dn /= Void; 
      do  
         check 
            c_ldap_delete(ldap_handle,dn.to_external); 
         end;
      end -- delete

feature {ANY} -- Status

   got_entry: BOOLEAN is 
      -- Have we done a search and got an entry?
      do  
         Result := ldap_got_search and ldap_got_entry;
      end -- got_entry
   
   got_search: BOOLEAN is 
      -- Have we succesfully done a search?
      do  
         Result := ldap_got_search;
      end -- got_search
   
   connected: BOOLEAN is 
      -- Are we connected to an LDAP server?
      do  
         Result := ldap_handle.is_not_null;
      end -- connected
   
   have_mods: BOOLEAN is 
      -- Do we have a mod list yet?  (for add/modify)
      do  
         Result := ldap_have_mods;
      end -- have_mods

	error_message: STRING is
		-- Get the last error message
		require
			connected;
		do
			Result.from_external_copy(c_ldap_err_msg(ldap_handle));
		end

feature {NONE} 
   -- C functions

   c_ldap_init(host: POINTER; port: INTEGER): POINTER is 
      -- The init
      external "C_WithoutCurrent"
      end -- c_ldap_init
   
   c_ldap_set_sb(ld, search_base: POINTER) is 
      -- set the search base
      external "C_WithoutCurrent"
      end -- c_ldap_set_sb
   
   c_ldap_set_timeout(ld: POINTER; timeout: INTEGER) is 
      -- set the search timeout
      external "C_WithoutCurrent"
      end -- c_ldap_set_timeout
   
   c_ldap_bind(ld, binddn, bindpw: POINTER): BOOLEAN is 
      -- Bind
      external "C_WithoutCurrent"
      end -- c_ldap_bind
   
   c_ldap_search(ld, filter: POINTER; scope: INTEGER): BOOLEAN is 
      -- Search
      external "C_WithoutCurrent"
      end -- c_ldap_search
   
   c_ldap_destroy(ld: POINTER) is 
      -- Free up everything
      external "C_WithoutCurrent"
      end -- c_ldap_destroy
   
   c_ldap_nresults(ld: POINTER): INTEGER is 
      -- Find out how many results there were from the last search
      external "C_WithoutCurrent"
      end -- c_ldap_nresults
   
   c_ldap_first_entry(ld: POINTER): BOOLEAN is 
      -- Get the first entry
      external "C_WithoutCurrent"
      end -- c_ldap_first_entry
   
   c_ldap_next_entry(ld: POINTER): BOOLEAN is 
      -- Get the next entry
      external "C_WithoutCurrent"
      end -- c_ldap_next_entry
   
   c_ldap_first_attribute(ld: POINTER): POINTER is 
      -- Get the first entry
      external "C_WithoutCurrent"
      end -- c_ldap_first_attribute
   
   c_ldap_next_attribute(ld: POINTER): POINTER is 
      -- Get the first entry
      external "C_WithoutCurrent"
      end -- c_ldap_next_attribute
   
   c_ldap_get_value(ld, attribute: POINTER; index: INTEGER): POINTER is 
      -- Get a specific value from an entry
      external "C_WithoutCurrent"
      end -- c_ldap_get_value
   
   c_ldap_compare(ld, dn, attr, value: POINTER): BOOLEAN is 
      -- Do an LDAP comparison.
      external "C_WithoutCurrent"
      end -- c_ldap_compare
   
   c_ldap_mod_add(ld, attr, value: POINTER; vlen: INTEGER) is 
      -- Add an attr/value for a doing an add/modify
      external "C_WithoutCurrent"
      end -- c_ldap_mod_add
   
   c_ldap_mod_replace(ld, attr, value: POINTER; vlen: INTEGER) is 
      -- Replace an attr/value for a doing an add/modify
      external "C_WithoutCurrent"
      end -- c_ldap_mod_replace
   
   c_ldap_mod_delete(ld, attr: POINTER) is 
      -- Delete an attr/value for a doing an add/modify
      external "C_WithoutCurrent"
      end -- c_ldap_mod_delete
   
   c_ldap_add(ld, dn: POINTER): BOOLEAN is 
      -- Add
      external "C_WithoutCurrent"
      end -- c_ldap_add
   
   c_ldap_modify(ld, dn: POINTER): BOOLEAN is 
      -- Add
      external "C_WithoutCurrent"
      end -- c_ldap_modify
   
   c_ldap_mod_clean(ld: POINTER) is 
      -- Add an attr/value for a doing an add
      external "C_WithoutCurrent"
      end -- c_ldap_mod_clean
   
   c_ldap_delete(ld, dn: POINTER): BOOLEAN is 
      -- delete
      external "C_WithoutCurrent"
      end -- c_ldap_delete

	c_ldap_err_msg(ld: POINTER): POINTER is
		-- Get the last error message
		external "C"
		end -- c_ldap_err_msg

end -- class LDAP
