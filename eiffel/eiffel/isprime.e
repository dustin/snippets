indexing
   description: "Prime number stuff.";
class PRIME

creation {ANY}
   make

feature {ANY}

   isprime(number: INTEGER): BOOLEAN is
      require
         big_enough: number > 1;
      local
         i: INTEGER;
         flag: BOOLEAN;
      do
         --  two is prime
         if number = 2 then
            Result := true;
         else
            -- make sure it's not an even number
            if not (number \\ 2 = 0) then
               from
                  i := 3;
               until
                  i * i > number or flag = true
               loop
                  if number \\ i = 0 then
                     flag := true;
                  end;
                  i := i + 2;
               end;
               if not flag then
                  Result := true;
               end;
            end;
         end;
      end -- isprime

   make is
      local
         i: INTEGER;
      do
         -- io.put_string("Making a prime thingy...%N");
         io.put_string("2%N");
         from
            i := 3;
         until
            i > 1000000
         loop
            if isprime(i) then
               io.put_integer(i);
               io.put_string("%N");
            end;
            i := i + 2;
         end;
      end -- make

end -- class PRIME
