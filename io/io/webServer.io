#!/usr/bin/env ioServer
#
# $Id: webServer.io,v 1.3 2003/08/20 17:33:12 dustin Exp $

WebServer = Object clone

WebServer buf = Buffer clone

WebServer hasRequest = method(
	/*
	"Checking to see if we have a request in\n" print
	buf print
	*/
	buf find("\r\n\r\n")
)

WebServer processRequest = method(aSocket, aServer,
	"Processing request\n" print
	lines = buf asString split("\r\n")
	query = lines at(0)
	lines removeAt(0)
	String join(List clone add("Query: ", query, "\n")) print
	lines print
	"\n" print
	rv=List clone add("HTTP/1.1 200 OK",
		"Content-type: text/plain",
		"Connection: close")
	aSocket write(String join(rv, "\r\n"))
	aSocket write("\r\n\r\n")
	aSocket write("Here's your content, mofo.\n")
	aSocket write("BTW, you requested " .. query)
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
