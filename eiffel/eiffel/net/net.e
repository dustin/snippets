-- Networking Classes

class TCP creation make

feature

	socket: INTEGER;

	listen(port: INTEGER): BOOLEAN is
		require port_too_large: port<65535;
			port_too_small: port>1;
		do
			socket:=do_listen(port);

			if(socket>=0) then
				Result:=true;
			end
		end -- listen

	do_listen(port: INTEGER): INTEGER is
		external "C" alias "getservsocket"
		end;

	make is
		do
			if (listen(8080)) then
				io.put_string("Got connected...%N");
				io.read_integer;
			else
				io.put_string("Didn't get connected...%N");
			end
		end
end
