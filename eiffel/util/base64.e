indexing
   description: "Base64 stuff.";
   author: "Dustin Sallings <dustin@spy.net>";
   version: "$Revision: 1.13 $";
   copyright: "1999";
   licensing: "EFLL <see forum.txt>";
class BASE64

feature {ANY} -- Actual encode/decode stuff

   encode(in: STRING): STRING is
      -- Base64 Encode.
      local
         ab, bb, cb, db, tmpa, tmpb: INTEGER_8;
         i, o: INTEGER;
         a, b, c: CHARACTER;
         second, third: BOOLEAN;
      do
         !!Result.make(256);
         from
            i := 1;
            o := 0;
         until
            i > in.count
         loop
            a := in.item(i);
            ab := a.to_integer;
            if in.valid_index(i + 1) then
               second := True;
               b := in.item(i + 1);
               bb := b.to_integer;
               if in.valid_index(i + 2) then
                  third := True;
                  c := in.item(i + 2);
                  cb := c.to_integer;
               else
                  third := False;
               end;
            else
               second := False;
            end;
            tmpa := ab;
            Result.add_last(get_char(tmpa |>> 2));
            tmpa := (ab & lasttwo) |<< 4;
            if second then
               tmpb := bb |>> 4;
               Result.add_last(get_char(tmpb | tmpa));
               tmpa := bb & lastfour;
               tmpa := tmpa |<< 2;
               if third then
                  tmpb := cb & firsttwo;
                  tmpb := tmpb |>> 6;
                  Result.add_last(get_char(tmpa | tmpb));
                  Result.add_last(get_char(cb & lastsix));
               else
                  Result.add_last(get_char(tmpa));
                  Result.add_last('=');
               end;
            else
               Result.add_last(get_char(tmpa));
               Result.add_last('=');
               Result.add_last('=');
            end;
            i := i + 3;
            o := o + 4;
            if o \\ 76 = 0 then
               -- Base64 says lines should be 76 characters.
               Result.add_last('%N');
            end;
         end;
      end -- encode

   decode(in: STRING): STRING is
      -- Base64 Decode.
      local
         ab, bb, cb, db, tmpa, tmpb: INTEGER_8;
         a, b, c: CHARACTER;
         first, second, third: BOOLEAN;
      do
         !!Result.make(256);
         from
            current_char := 0;
         until
            current_char >= in.count
         loop
            if find_next(in) then
               ab := the_bits(in);
               first := True;
            else
               first := False;
            end;
            if find_next(in) then
               bb := the_bits(in);
            else
               first := False;
            end;
            if find_next(in) then
               if is_padding(in) then
                  second := False;
               else
                  cb := the_bits(in);
                  second := True;
               end;
            end;
            if find_next(in) then
               if is_padding(in) then
                  third := False;
               else
                  db := the_bits(in);
                  third := True;
               end;
            end;
            if first then
               -- first byte
               tmpa := ab |<< 2;
               tmpb := bb |>> 4 & lasttwo;
               Result.add_last((tmpa | tmpb).to_character);
               if second then
                  -- second byte
                  tmpa := bb |<< 4;
                  tmpb := cb |>> 2 & lastfour;
                  Result.add_last((tmpa | tmpb).to_character);
                  if third then
                     -- third byte
                     Result.add_last((db | (cb |<< 6)).to_character);
                  end;
               end;
            end;
         end;
      end -- decode

feature {NONE}

   the_bits(s: STRING): INTEGER_8 is
      -- The bits of the current data
      require
         current_char <= s.count;
      local
         i: INTEGER;
      do
         i := charmap.index_of(s.item(current_char), 1) - 1;
         Result := truncate(i);
      end -- the_bits

   is_padding(s: STRING): BOOLEAN is
      do
         Result := s.item(current_char).is_equal('=');
      end -- is_padding

   find_next(s: STRING): BOOLEAN is
      -- Find the next decodeable character
      do
         current_char := current_char + 1;
         from
            Result := False;
         until
            current_char > s.count or Result = True
         loop
            if charmap.has(s.item(current_char)) then
               Result := True;
            else
               current_char := current_char + 1;
            end;
         end;
      end -- find_next

   get_char(b: INTEGER_8): CHARACTER is
      -- Get the character this bit pattern represents.
      local
         i: INTEGER;
      do
         i := b.to_integer;
         Result := charmap.item(i + 1);
      end -- get_char

   truncate(in: INTEGER_32): INTEGER_8 is
      -- Truncate a INTEGER_32 to a INTEGER_8
      do
         Result := in.to_integer_8;
      end -- truncate

   lasttwo:INTEGER_8 is
	once
		Result := ("00000011").binary_to_integer.to_character.to_integer;
	end

   lastfour:INTEGER_8 is
	once
		Result := ("00001111").binary_to_integer.to_character.to_integer;
	end

   lastsix:INTEGER_8 is
	once
		Result := ("00111111").binary_to_integer.to_character.to_integer;
	end

   firsttwo:INTEGER_8 is
	once
		Result := ("11000000").binary_to_integer.to_character.to_integer;
	end

   current_char: INTEGER;

   base64_isinitialized: BOOLEAN;

   charmap: STRING is "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

end -- class BASE64
