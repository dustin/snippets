indexing
	description: "Base64 stuff.";

class BASE64

creation
	make

feature
	-- Constructors

	make is
		local
			s: STRING;
		do
			s:=encode("ABC");
			io.put_string("ABC encoded is ");
			io.put_string(s);
			io.put_string("%N");
		end

feature {ANY}
	-- Actual encode/decode stuff
	encode(in: STRING): STRING is
		local
			ab, bb, cb, db, tmpa, tmpb, lasttwo,
				lastfour, lastsix, firsttwo, outa, outb, outc, outd: BIT 8;
			i: INTEGER;
			a, b, c: CHARACTER;
		do
			!!Result.make(0);
			Result.clear;

			lasttwo:= truncate ((("00000011").binary_to_integer).to_bit);
			lastfour:= truncate ((("00001111").binary_to_integer).to_bit);
			lastsix:= truncate((("00111111").binary_to_integer).to_bit);
			firsttwo:= truncate((("11000000").binary_to_integer).to_bit);

			from
				i:=1;
			until
				i>in.count
			loop
				a:=in.item(i);
				b:=in.item(i+1);
				c:=in.item(i+2);

				ab:=a.to_bit;
				bb:=b.to_bit;
				cb:=c.to_bit;

				-- start on first byte
				tmpa:=ab;
				outa:= (tmpa @>> 2);

				-- start on second byte
				tmpa:= (ab and lasttwo) @<< 4;
				tmpb:= bb @>> 4;
				outb:= (tmpb or tmpa);

				-- start on third byte
				tmpa:=bb and lastfour;
				tmpa:= tmpa @<< 2;

				tmpb:=cb and firsttwo;
				tmpb:= tmpb @>> 6;

				outc:= (tmpa or tmpb);

				-- start on fourth byte
				outd:= (cb and lastsix) ;

				display(outa, outb, outc, outd);

				i:=i+3;
			end
		end

	decode(in: STRING): STRING is
		do
		end

feature {NONE}

	display(a, b, c, d: BIT 8) is
		local
			i: INTEGER;
		do
			i:=a.to_integer;
			io.put_character(charmap.item(i+1));

			i:=b.to_integer;
			io.put_character(charmap.item(i+1));

			i:=c.to_integer;
			io.put_character(charmap.item(i+1));

			i:=d.to_integer;
			io.put_character(charmap.item(i+1));

			io.put_string("%N");
		end

	truncate(in: BIT 32): BIT 8 is
		do
			Result:=((in.to_integer).to_character).to_bit;
		end

	charmap: STRING is
	  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
end
