#!/usr/bin/env ioServer

# Comment

Echo = Object clone
Echo handleSocketFromServer = method(aSocket, aServer,
	write("[Got echo connection from ", aSocket host, "]\n")
	while(aSocket isOpen,
		if(aSocket read, aSocket write(aSocket readBuffer asString))
	)
	write("[Closed ", aSocket host, "]\n")
)

write("[Starting echo server on port 8456]\n")
server = Server clone setPort(8456)
server handleSocket = method(aSocket,
	Echo clone @handleSocketFromServer(aSocket, self)
)
server start
