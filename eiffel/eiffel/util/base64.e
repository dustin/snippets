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
				lastfour, lastsix, firsttwo: BIT 32;
			outa, outb, outc, outd: BIT 8;
			i: INTEGER;
			a, b, c: CHARACTER;
		do
			!!Result.make(0);
			Result.clear;

			lasttwo:=(("00000011").binary_to_integer).to_bit;
			lastfour:=(("00001111").binary_to_integer).to_bit;
			lastsix:=(("00111111").binary_to_integer).to_bit;
			firsttwo:=(("11000000").binary_to_integer).to_bit;

			io.put_string(firsttwo.to_string);
			io.put_string("%N");

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

				io.put_string(ab.to_string);
				io.put_string("%N");
				io.put_string(bb.to_string);
				io.put_string("%N");
				io.put_string(cb.to_string);
				io.put_string("%N------------%N");

				-- start on first byte
				tmpa:=ab;
				outa:= truncate (tmpa @>> 2);

				io.put_string(outa.to_string);
				io.put_string("%N");

				-- start on second byte
				tmpb:=ab and lasttwo;
				tmpb:= tmpb @<< 6;

				tmpa:=bb;
				tmpb:= tmpb @>> 4;

				outb:= truncate (tmpb or tmpa);

				io.put_string(outb.to_string);
				io.put_string("%N");

				-- start on third byte
				tmpa:=bb and lastfour;
				tmpa:= tmpa @<< 2;

				tmpb:=cb and firsttwo;
				tmpb:= tmpb @>> 6;

				outc:= truncate (tmpa or tmpb);

				io.put_string(outc.to_string);
				io.put_string("%N");

				-- start on fourth byte
				outd:= truncate (cb and lastsix) ;

				io.put_string(outd.to_string);
				io.put_string("%N");

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

			-- i:=b.to_integer;
			-- io.put_character(charmap.item(i+1));

			i:=c.to_integer;
			io.put_character(charmap.item(i+1));

			-- i:=d.to_integer;
			-- io.put_character(charmap.item(i+1));

			io.put_string("%N");
		end

	truncate(in: BIT 32): BIT 8 is
		do
			Result:=((in.to_integer).to_character).to_bit;
		end

	charmap: STRING is
	  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
end
