#!/usr/bin/env ioServer
#
# $Id: webServer.io,v 1.4 2003/08/20 20:14:01 dustin Exp $

WebServer = Object clone

WebServer buf = Buffer clone

WebServer hasRequest = method(
	/*
	write("Checking to see if we have a request in\n")
	write(buf)
	*/
	buf find("\r\n\r\n")
)

WebServer processRequest = method(aSocket, aServer,
	write("Processing request\n")
	lines = buf asString split("\r\n")
	query = lines at(0)
	lines removeAt(0)
	write("Query:  " .. query .. "\n")
	lines print
	write("\n")
	rv=List clone add("HTTP/1.1 200 OK",
		"Content-type: text/plain",
		"Connection: close")
	aSocket write(String join(rv, "\r\n"))
	aSocket write("\r\n\r\n")
	aSocket write("Here's your content, mofo.\n")
	aSocket write("BTW, you requested " .. query .. "\n")
	aSocket close
	buf empty
)

WebServer handleSocketFromServer = method(aSocket, aServer,
	write("[Got echo connection from ", aSocket host, "]\n")
	while(aSocket isOpen,
		if(aSocket read,
			buf append(aSocket readBuffer)
			if(hasRequest, processRequest(aSocket, aServer))
			aSocket readBuffer empty)
	)
	write("[Closed ", aSocket host, "]\n")
)

write("[Starting echo server on port 8456]\n")
server = Server clone setPort(8456)
server handleSocket = method(aSocket,
	WebServer clone @handleSocketFromServer(aSocket, self)
)
server start
