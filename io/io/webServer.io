#!/usr/bin/env ioServer
#
# $Id: webServer.io,v 1.8 2003/08/21 21:09:52 dustin Exp $

//
// Web request
//

WebRequest = Object clone

WebRequest headers = Nil
WebRequest reqMethod = Nil
WebRequest path = Nil
WebRequest queryString = Nil
WebRequest version = Nil

// This not only parses the request, but modifies the buffer to remove all
// of the non-header information
WebRequest parseRequest = method(buf,
	// Copy the buffer and remove everything after the end of the headers
	btmp = buf fromTo(0, buf find("\r\n\r\n") - 1)
	bextra = buf fromTo(buf find("\r\n\r\n") + 4, buf length - 1)
	buf empty
	buf .. bextra
	lines = btmp asString split("\r\n")
	// The first line is the query
	self path = lines at(0)
	self reqMethod = path substring(0, path find(" ") - 1)
	self version = path substring(path reverseFind(" ") + 1)
	self path = path substring(path find(" ") + 1,
		path reverseFind(" ") - 1)
	// Check for a query string
	if(self path find("?"),
		self queryString = path substring(path find("?") + 1)
		self path = path substring(0, path find("?") - 1),
		self queryString = ""
	)

	lines removeAt(0)
	// Parse the headers
	self headers = Map clone
	lines doBlock(block(s,
		idx = s find(": ", 0)
		if(idx == Nil,
			write("No colon in " .. s .. "\n")
			lines print
			headers print)
		k = s substring(0, idx-1)
		v = s substring(idx + 2)
		self headers atPut(k, v)
		))
	self
)

//
// URL Handlers
//

// The default handler provides functionality for sending responses, but
// just issues 404s
WebUrlHandler = Object clone

WebUrlHandler setResponseHeaders = method(request, aSocket, status, headers,
	v = request version
	statusName = WebServer responseStatusCodes at(status)
	statusString = String join(list(v, status, statusName), " ")
	lheaders = list(statusString,
		"Connection: close")
	if(headers != Nil,
		headers foreach(hk, hv, lheaders add(hk .. ": " hv)),
		lheaders add("Content-type: text/plain"))
	aSocket write(String join(lheaders, "\r\n"))
	aSocket write("\r\n\r\n")
)

WebUrlHandler handleRequest = method(req, socket,
	raiseException("WebServer.ProtocolError.404",
		"No handler for " ..(req path))
)

// Error handler

WebErrorHandler = WebUrlHandler clone

WebErrorHandler handleRequest = method(req, socket,
	raiseException("WebServer.ProtocolError.405",
		"Error handlers don't provide content")
)

WebErrorHandler handleError = method(request, aSocket, eName, eDescription,
	status = eName substring(24)
	statusName = WebServer responseStatusCodes at(status)
	setResponseHeaders(request, aSocket, status, Nil)
	// Send the error
	aSocket write(status, " - ", statusName, "\r\n")
	aSocket write(eDescription .. "\n")
	status
)

// A happier handler

WebUrlTimeHandler = WebUrlHandler clone

WebUrlTimeHandler handleRequest = method(req, socket,
	setResponseHeaders(req, socket, "200", Nil)
	d = Date clone now
	ds = d asString("%Y/%m/%d %X")
	socket write("The current time is ", ds)
	"200"
)

// Standard web handling
WebUrlStdHandler = WebUrlHandler clone

WebUrlStdHandler parseGet = method(req, socket,
	// write("Parsing GET from ", req queryString, "\n")
	CGI clone parseString(req queryString)
)

WebUrlStdHandler parsePost = method(req, socket,
	// write("Parsing POST\n")
	length = req headers at("Content-Length")
	// write("Post length is ", length, "\n")
	while(socket readBuffer length < length,
		socket read)
	btmp = socket readBuffer fromTo(0, length - 1)
	// write("Read length:  ", btmp length, "\n")
	s = String clone append(btmp asString)
	CGI clone parseString(s)
)

WebUrlStdHandler getParameters = method(req, socket,
	rv = Nil
	m = req reqMethod
	if(m beginsWith("GET"),
		rv = parseGet(req, socket))
	if(m beginsWith("POST"),
		rv = parsePost(req, socket))
	rv
)

// Print out debug stuff

WebUrlDebugHandler = WebUrlStdHandler clone
WebUrlDebugHandler handleRequest = method(req, socket,
	setResponseHeaders(req, socket, "200", Nil)
	socket write("Method is ", req reqMethod, "\n")
	socket write("Path is ", req path, "\n")
	socket write("Query string is ", req queryString, "\n")
	cgi = getParameters(req, socket)
	if(cgi count > 0,
		cgi foreach(k, v, socket write("\t", k, " = ", v, "\n")))
	socket write("Headers:\n")
	req headers foreach(k, v, socket write("\t", k, " = ", v, "\n"))
	"200"
)

//
// Begin the web server itself
//

WebServer = Object clone

WebServer handlers = Map clone

WebServer responseStatusCodes = Map clone

WebServer initialize = method(
	self responseStatusCodes atPut("100", "Continue")
	self responseStatusCodes atPut("101", "Switching Protocols")
	self responseStatusCodes atPut("200", "OK")
	self responseStatusCodes atPut("201", "Created")
	self responseStatusCodes atPut("202", "Accepted")
	self responseStatusCodes atPut("203", "Non-Authoritative Information")
	self responseStatusCodes atPut("204", "No Content")
	self responseStatusCodes atPut("205", "Reset Content")
	self responseStatusCodes atPut("206", "partial Content")
	self responseStatusCodes atPut("300", "Multiple Choices")
	self responseStatusCodes atPut("301", "Moved Permanently")
	self responseStatusCodes atPut("302", "Found")
	self responseStatusCodes atPut("303", "See Other")
	self responseStatusCodes atPut("304", "Not Modified")
	self responseStatusCodes atPut("305", "use Proxy")
	self responseStatusCodes atPut("307", "Temporary Redirect")
	self responseStatusCodes atPut("400", "Bad Request")
	self responseStatusCodes atPut("401", "Unauthorized")
	self responseStatusCodes atPut("402", "Payment Required")
	self responseStatusCodes atPut("403", "Forbidden")
	self responseStatusCodes atPut("404", "Not Found")
	self responseStatusCodes atPut("405", "Method Not Allowed")
	self responseStatusCodes atPut("406", "Not Acceptable")
	self responseStatusCodes atPut("407", "Proxy Authentication Required")
	self responseStatusCodes atPut("408", "Request Time-out")
	self responseStatusCodes atPut("409", "Conflict")
	self responseStatusCodes atPut("410", "Gone")
	self responseStatusCodes atPut("411", "Length Required")
	self responseStatusCodes atPut("412", "Precondition Failed")
	self responseStatusCodes atPut("413", "Request Entity Too Large")
	self responseStatusCodes atPut("414", "Request-URI Too Large")
	self responseStatusCodes atPut("415", "Unsupported Media Type")
	self responseStatusCodes atPut("416", "Request range not satisfiable")
	self responseStatusCodes atPut("417", "Expectation Failed")
	self responseStatusCodes atPut("500", "Internal Server Error")
	self responseStatusCodes atPut("501", "Not Implemented")
	self responseStatusCodes atPut("502", "Bad Gateway")
	self responseStatusCodes atPut("503", "Service Unavailable")
	self responseStatusCodes atPut("504", "Gateway Time-out")
	self responseStatusCodes atPut("505", "HTTP Version not supported")
	self
)

WebServer defaultHandler = WebUrlHandler clone
WebServer errorHandler = WebErrorHandler clone

WebServer setDefaultHandler = method(handler,
	self defaultHandler = handler
)

WebServer setErrorHandler = method(handler,
	self errorHandler = handler
)

WebServer setHandler = method(path, handler,
	self handlers atPut(path, handler)
)

WebServer hasRequest = method(buf,
	buf find("\r\n\r\n")
)

// Main loop that processes request
WebServer processRequest = method(aSocket, aServer,
	request = WebRequest clone parseRequest(aSocket readBuffer)
	handler = self handlers at(request path)
	if(handler == Nil, handler = self defaultHandler)

	status = Nil
	catchException("WebServer.ProtocolError",
		status = handler handleRequest(request, aSocket),
		status = errorHandler handleError(request, aSocket,
			exceptionName, exceptionDescription))
	/*
	ds = Date clone now asString("[%d/%b/%Y:%X %Z]")
	write(aSocket host, " - - ", ds, " \"", request reqMethod, " ",
		request path, " ", request version, "\" ", status, " 0\n")
	*/
	aSocket close
)

//
// End of WebServer implementation
//

WebServer handleSocketFromServer = method(aSocket, aServer,
	write("[New Request ", aSocket host, "]\n")
	while(aSocket isOpen,
		if(aSocket read,
			if(hasRequest(aSocket readBuffer),
				processRequest(aSocket, aServer)
				aSocket readBuffer empty
			)
		)
	)
	write("[Closed ", aSocket host, "]\n")
)

//
// -- The beginning --
//

write("[Starting web server on port 8456]\n")
server = Server clone setPort(8456)
ws = WebServer clone initialize
ws setHandler("/time", WebUrlTimeHandler clone)
ws setHandler("/debug", WebUrlDebugHandler clone)
server handleSocket = method(aSocket,
	ws @handleSocketFromServer(aSocket, self)
)
server start
