indexing
   description: "Base64 stuff.";
class BASE64

creation {ANY}
   make

feature {ANY} -- Constructors

   make is
      -- Initialization
      local
         e, d: STRING;
      do
         init_const;
            -- io.put_string("Encoded:%N");
            -- e:=encode(argument(1));
            -- io.put_string(e);
            -- io.put_string("%N");
            -- io.put_string("Decoded:%N");
            -- d:=decode(e);
            -- io.put_string(d);
            -- io.put_string("%N");

      end -- make

feature {ANY} -- Actual encode/decode stuff

   encode(in: STRING): STRING is
      -- Base64 Encode.
      local
         ab, bb, cb, db, tmpa, tmpb: BIT 8;
         i, o: INTEGER;
         a, b, c: CHARACTER;
         second, third: BOOLEAN;
      do
         !!Result.make(0);
         Result.clear;
         from
            i := 1;
            o := 0;
         until
            i > in.count
         loop
            a := in.item(i);
            ab := a.to_bit;
            if in.valid_index(i + 1) then
               second := true;
               b := in.item(i + 1);
               bb := b.to_bit;
               if in.valid_index(i + 2) then
                  third := true;
                  c := in.item(i + 2);
                  cb := c.to_bit;
               else
                  third := false;
               end;
            else
               second := false;
            end;
            tmpa := ab;
            Result.add_last(get_char(tmpa @>> 2));
            tmpa := (ab and lasttwo) @<< 4;
            if second then
               tmpb := bb @>> 4;
               Result.add_last(get_char(tmpb or tmpa));
               tmpa := bb and lastfour;
               tmpa := tmpa @<< 2;
               if third then
                  tmpb := cb and firsttwo;
                  tmpb := tmpb @>> 6;
                  Result.add_last(get_char(tmpa or tmpb));
                  Result.add_last(get_char(cb and lastsix));
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
         ab, bb, cb, db, tmpa, tmpb: BIT 8;
         o: INTEGER;
         a, b, c: CHARACTER;
         first, second, third: BOOLEAN;
      do
         !!Result.make(0);
         Result.clear;
         from
            current_char := 0;
            o := 0;
         until
            current_char >= in.count
         loop
            if find_next(in) then
               ab := the_bits(in);
               first := true;
            else
               first := false;
            end;
            if find_next(in) then
               bb := the_bits(in);
            else
               first := false;
            end;
            if find_next(in) then
               if is_padding(in) then
                  second := false;
               else
                  cb := the_bits(in);
                  second := true;
               end;
            end;
            if find_next(in) then
               if is_padding(in) then
                  third := false;
               else
                  db := the_bits(in);
                  third := true;
               end;
            end;
            if first then
               -- first byte
               tmpa := ab @<< 2;
               tmpb := bb @>> 4 and lasttwo;
               Result.add_last((tmpa or tmpb).to_character);
               if second then
                  -- second byte
                  tmpa := bb @<< 4;
                  tmpb := cb @>> 2 and lastfour;
                  Result.add_last((tmpa or tmpb).to_character);
                  if third then
                     -- third byte
                     Result.add_last((db or cb @<< 6).to_character);
                  end;
               end;
            end;
         end;
      end -- decode

feature {NONE}

   the_bits(s: STRING): BIT 8 is
      -- The bits of the current data
      require
         current_char <= s.count;
      local
         i: INTEGER;
      do
         i := charmap.index_of(s.item(current_char)) - 1;
         Result := truncate(i.to_bit);
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
            Result := false;
         until
            current_char > s.count or Result = true
         loop
            if charmap.has(s.item(current_char)) then
               Result := true;
            else
               current_char := current_char + 1;
            end;
         end;
      end -- find_next

   get_char(b: BIT 8): CHARACTER is
      -- Get the character this bit pattern represents.
      local
         i: INTEGER;
      do
         i := b.to_integer;
         Result := charmap.item(i + 1);
      end -- get_char

   truncate(in: BIT 32): BIT 8 is
      -- Truncate a BIT 32 to a BIT 8
      do
         Result := in.to_integer.to_character.to_bit;
      end -- truncate

   init_const is
      -- Set up some constants, can't find a better way.
      once
         lasttwo := truncate(("00000011").binary_to_integer.to_bit);
         lastfour := truncate(("00001111").binary_to_integer.to_bit);
         lastsix := truncate(("00111111").binary_to_integer.to_bit);
         firsttwo := truncate(("11000000").binary_to_integer.to_bit);
      end -- init_const

   lasttwo: BIT 8;

   lastfour: BIT 8;

   lastsix: BIT 8;

   firsttwo: BIT 8;

   current_char: INTEGER;

   charmap: STRING is "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";

end -- class BASE64
