indexing
   description: "CGI Processing routines.";
class CGI

creation {ANY}
   make, parse

feature {ANY}

   make is
      -- Standalone, we simply parse the data, and print it back out.
      local
         c: CHARACTER;
      do
         parse;
         display;
      end -- make

   display is
      -- Display our data.
	  require
		have_cgi_data;
      local
         i: INTEGER;
         kv: KEYVALUE;
      do
         from
            i := 1;
         until
            i > cgi_data.count
         loop
            kv := cgi_data @ i;
            io.put_string(kv.key);
            io.put_string(" -> ");
            io.put_string(kv.value);
            io.put_string("%N");
            i := i + 1;
         end;
      end -- display

   lookup(key: STRING): STRING is
      -- Get the value for a key
      require
         key /= Void;
		 have_cgi_data;
      local
         i: INTEGER;
         kv: KEYVALUE;
      do
         from
            i := 1;
         until
            i > cgi_data.count or Result /= Void
         loop
            kv := cgi_data.item(i);
            if kv.key.is_equal(key) then
               Result := kv.value;
            end;
            i := i + 1;
         end;
      ensure
         Result /= Void;
      end -- lookup

   parse is
      -- Parse the CGI data, only done once.
      local
         a, b: ARRAY[STRING];
         kv: KEYVALUE;
         s: STRING;
         i: INTEGER;
      once
         get_cgi_data;
         a := cgi_input.split_on('&');
         !!cgi_data.with_capacity(1,1);
         from
            i := 1;
         until
            i > a.count
         loop
            s := a @ i;
            b := s.split_on('=');
            !!kv.make(decode_string(b.item(1)),decode_string(b.item(2)));
            cgi_data.add_last(kv);
            i := i + 1;
         end;
      end -- parse

	have_cgi_data: BOOLEAN is
		-- Verify we have parsed the CGI data.
		do
			Result:= ( cgi_data /= Void);
		end

feature {ANY} -- Misc features, probably should be in a utility class

   decode_string(in: STRING): STRING is
      -- Decode an HTTP encoded string.
      local
         i: INTEGER;
      do
         !!Result.make(1);
         from
            i := 1;
         until
            i > in.count
         loop
            if in.item(i) = '+' then
               Result.add_last(' ');
            elseif in.item(i) = '%%' then
               i := i + 1;
               Result.add_last(from_hex(in.substring(i,i + 2)));
               i := i + 1;
            else
               Result.add_last(in.item(i));
            end;
            i := i + 1;
         end;
      end -- decode_string

   from_hex(in: STRING): CHARACTER is
      -- Take a two digit hex string, and convert it to a hex character.
      require
         in.item(1).is_hex_digit;
         in.item(2).is_hex_digit;
      local
         a, b: INTEGER;
         map, tmp: STRING;
      do
         !!tmp.copy(in);
         tmp.to_upper;
         map := "0123456789ABCDEF";
         a := map.index_of(tmp.item(1)) - 1;
         b := map.index_of(tmp.item(2)) - 1;
         a := a * 16 + b;
         Result := a.to_character;
      end -- from_hex

feature {CGI}

   -- The parsed data.
   cgi_data: ARRAY[KEYVALUE];

   -- The actual input.
   cgi_input: STRING;

   get_cgi_data is
      -- Find the CGI form data and return it.
      local
         tmp: STRING;
         i, top: INTEGER;
      once
         tmp := get_environment_variable("REQUEST_METHOD");
         if tmp /= Void then
            if tmp.is_equal("GET") then
               cgi_input := get_environment_variable("QUERY_STRING");
            elseif tmp.is_equal("POST") then
               tmp := get_environment_variable("CONTENT_LENGTH");
               if tmp /= Void then
                  from
                     top := tmp.to_integer;
                     i := 1;
                     !!cgi_input.make(1);
                  until
                     i > top
                  loop
                     std_input.read_character;
                     cgi_input.add_last(std_input.last_character);
                     i := i + 1;
                  end;
               end;
            end;
         end;
      ensure
         cgi_input /= Void;
      end -- get_cgi_data

end -- class CGI
