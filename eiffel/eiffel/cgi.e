indexing
   description: "CGI Processing routines.";
   author: "Dustin Sallings <dustin@spy.net>";
   version: "$Revision: 1.7 $";
   copyright: "1999";
   license: "See forum.txt";
class CGI
   -- A simple CGI data handling class.  Probably slow for large amounts of
   -- data, doesn't handle multipart form/data (yet, I need it to), and
   -- doesn't handle multiple values per key (not directly anyway).

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
         has_data;
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
         has_data;
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
		 tried: BOOLEAN;
      once
		 if not tried then
         get_cgi_data;
         a := split_on(cgi_input,'&');
         !!cgi_data.with_capacity(1,1);
         from
            i := 1;
         until
            i > a.count
         loop
            s := a @ i;
            b := split_on(s,'=');
            !!kv.make(decode_string(b.item(1)),decode_string(b.item(2)));
            cgi_data.add_last(kv);
            i := i + 1;
         end;
		 has_data:=true;
		 end
	  rescue
		tried:=true;
		retry;
      end -- parse

   has_data: BOOLEAN;

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
         in.item(1).is_hexadecimal_digit;
         in.item(2).is_hexadecimal_digit;
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

feature {ANY} -- Features that really belong in a string class.

   split_on(s: STRING; on: CHARACTER): ARRAY[STRING] is
      -- split a string on a given character.
      local
         split_buffer: ARRAY[STRING];
      do
         !!split_buffer.with_capacity(4,1);
         if s.count > 0 then
            split_buffer.clear;
            split_on_in(s,split_buffer,on);
            if not split_buffer.empty then
               Result := split_buffer.twin;
            end;
         end;
      end -- split_on

   split_on_in(s: STRING; words: COLLECTION[STRING]; on: CHARACTER) is
      -- A version of split_in that doesn't assume it knows how
      -- you want to split.
      require
         words /= Void;
      local
         state, i: INTEGER;
         c: CHARACTER;
         tmp_string: STRING;
      do
         if s.count > 0 then
            !!tmp_string.make(256);
            from
               i := 1;
            until
               i > s.count
            loop
               c := s.item(i);
               if state = 0 then
                  if not (c = on) then
                     tmp_string.clear;
                     tmp_string.extend(c);
                     state := 1;
                  end;
               else
                  -- state is not 0, looking for the end
                  if c = on then
                     words.add_last(tmp_string.twin);
                     state := 0;
                  else
                     -- this is the one for which we are searching
                     tmp_string.extend(c);
                  end;
               end;
               i := i + 1;
            end;
            if state = 1 then
               words.add_last(tmp_string.twin);
            end;
         end;
      end -- split_on_in

feature {CGI} -- The parsed data.

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
