indexing
   description: "Network library $Revision: 1.4 $";
-- Networking Classes
--
-- $Id: net.e,v 1.4 1999/05/18 08:20:24 dustin Exp $
class TCP

creation {ANY}
   make

feature {ANY}

   socket: INTEGER;

   listen(port: INTEGER): BOOLEAN is
      -- Create a listening socket on the given port.
      require
         port_too_large: port < 65535;
         port_too_small: port > 1;
      do
         socket := do_listen(port);
         if socket >= 0 then
            Result := true;
         end;
      end -- listen

   make is
      do
         if listen(8080) then
            io.put_string("Got connected...%N");
            io.read_integer;
         else
            io.put_string("Didn't get connected...%N");
         end;
      end -- make

feature {NONE}

   do_listen(port: INTEGER): INTEGER is
      external "C_WithoutCurrent"
      alias "getservsocket"
      end -- do_listen

end -- class TCP
