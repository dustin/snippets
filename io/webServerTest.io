#!/usr/bin/env ioServer

doFile("webServer.io")
doFile("xmlrpc.io")

//
// -- The beginning --
//

// Make an XMLRPC handler and create an XMLRPC serverto handle it

SimpleMethods := Object clone

SimpleMethods testInt := method(i,
	"I got a " ..(i asString)
)

xmlrpc := XmlRpcServer clone
xmlrpc addHandler("test", SimpleMethods clone)

// OK, now let's make an XMLRPC web handler to call invokeXML

XMLRPCWebHandler := WebUrlStdHandler clone

XMLRPCWebHandler handleRequest := method(req, socket,
	socket addHeader("Content-Type", "text/xml")
	length := req headers at("Content-Length") asNumber
	xml := readFromBody(req, socket, length)
	write("Received\n", xml, "\n")
	rv := xmlrpc invokeXML(xml)
	write("Returning\n", rv, "\n")
	socket addHeader("Content-Length", rv length asString)
	socket write(rv)
)

write("[Starting web server on port 8456]\n")
server := Server clone setPort(8456)
ws := WebServer clone
ws addHandler("/RPC2", XMLRPCWebHandler clone)
ws addHandler("/time", WebUrlTimeHandler clone)
ws addHandler("/debug", WebUrlDebugHandler clone)
ws addHandler("/debug/*", WebUrlDebugHandler clone)
server handleSocket := method(aSocket,
	ws @handleSocketFromServer(aSocket, self)
)
server start
