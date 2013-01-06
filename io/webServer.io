#!/usr/bin/env ioServer
#
# $Id: webServer.io,v 1.14 2003/08/29 09:15:42 dustin Exp $

// Object definitions

// The web server
WebServer := Object clone
// Request
WebRequest := Object clone
// Response
WebResponse := Object clone
// Content handlers
WebUrlHandler := Object clone
WebUrlStdHandler := WebUrlHandler clone
WebUrlDebugHandler := WebUrlStdHandler clone
WebErrorHandler := WebUrlHandler clone
WebUrlTimeHandler := WebUrlHandler clone

//
// Web request
//

WebRequest headers := nil
WebRequest reqMethod := nil
WebRequest path := nil
WebRequest queryString := nil
WebRequest httpVersion := nil

// This not only parses the request, but modifies the buffer to remove all
// of the non-header information
WebRequest parseRequest := method(buf,
	// Copy the buffer and remove everything after the end of the headers
	btmp := buf exSlice(0, buf findSeq("\r\n\r\n") - 1)
	bextra := buf exSlice(buf findSeq("\r\n\r\n") + 4, buf size - 1)
	buf empty
	buf .. bextra
	lines := btmp asString split("\r\n")
	// The first line is the query
	self path := lines at(0)
	self reqMethod := path exSlice(0, path findSeq(" ") - 1)
	self httpVersion := path exSlice(path reverseFindSeq(" ") + 1)
	self path := path exSlice(path findSeq(" ") + 1,
		path reverseFindSeq(" ") - 1)
	// Check for a query string
	if(self path findSeq("?"),
		self queryString := path exSlice(path findSeq("?") + 1)
		self path := path exSlice(0, path findSeq("?") - 1),
		self queryString := ""
	)

	lines removeAt(0)
	// Parse the headers
	self headers := Map clone
	lines foreach(s,
		idx := s findSeq(": ", 0)
		if(idx == nil,
			write("No colon in " .. s .. "\n")
			lines print
			headers print)
		k := s exSlice(0, idx-1)
		v := s exSlice(idx + 2)
		self headers atPut(k, v)
		)
	self
)

//
// Web server response socket
//
// this is a proxy object that wraps a socket and provides some niceities
//

WebResponse bytesWritten := 0
WebResponse socket := nil

// Additional headers
WebResponse headers := nil
// Response status
WebResponse status := nil
// protocol version
WebResponse httpVersion := nil
WebResponse beginning := 1

WebResponse init := method(
	self bytesWritten := 0
	self headers := Map clone
	self headers atPut("Connection", "close")
	self status := "200"
	self httpVersion := "HTTP/1.0"
	self
)

WebResponse addHeader := method(k, v,
	self headers atPut(k, v)
	self
)
WebResponse setStatus := method(status,
	self status := status
	self
)
WebResponse setHttpVersion := method(v,
	self httpVersion := v
	self
)
WebResponse setSocket := method(s,
	self socket := s
	self
)

WebResponse writeHeaders := method(
	statusName := WebServer responseStatusCodes at(status)
	statusString := String join(list(httpVersion, status, statusName), " ")
	lheaders := list(statusString)
	headers foreach(hk, hv, lheaders add(hk .. ": " .. hv))
	internalWriteString(String join(lheaders, "\r\n"))
	internalWriteString("\r\n\r\n")
	self
)

WebResponse write := method(
	if(beginning,
		writeHeaders
		self beginning := nil
	)
	internalWriteList(thisMessage argsEvaluatedIn(sender))
	self
)

WebResponse internalWriteList := method(l,
	l foreach(t, internalWriteString(t))
	self
)

WebResponse internalWriteString := method(s,
	self bytesWritten +=(s length)
	self socket write(s)
	self
)

WebResponse forward := method(
	methodName := thisMessage name
	// write("Performing ", methodName, " via the proxy\n")
	args := thisMessage argsEvaluatedIn(sender)
	socket performWithArgList(methodName, args)
)

// These would be processed by the proxy, but I'd rather avoid it
WebResponse readBuffer := method(socket readBuffer)

//
// Begin the web server itself
//


WebServer handlers := nil

WebServer responseStatusCodes := Map clone
// Begin status codes
WebServer responseStatusCodes atPut("100", "Continue")
WebServer responseStatusCodes atPut("101", "Switching Protocols")
WebServer responseStatusCodes atPut("200", "OK")
WebServer responseStatusCodes atPut("201", "Created")
WebServer responseStatusCodes atPut("202", "Accepted")
WebServer responseStatusCodes atPut("203", "Non-Authoritative Information")
WebServer responseStatusCodes atPut("204", "No Content")
WebServer responseStatusCodes atPut("205", "Reset Content")
WebServer responseStatusCodes atPut("206", "partial Content")
WebServer responseStatusCodes atPut("300", "Multiple Choices")
WebServer responseStatusCodes atPut("301", "Moved Permanently")
WebServer responseStatusCodes atPut("302", "Found")
WebServer responseStatusCodes atPut("303", "See Other")
WebServer responseStatusCodes atPut("304", "Not Modified")
WebServer responseStatusCodes atPut("305", "use Proxy")
WebServer responseStatusCodes atPut("307", "Temporary Redirect")
WebServer responseStatusCodes atPut("400", "Bad Request")
WebServer responseStatusCodes atPut("401", "Unauthorized")
WebServer responseStatusCodes atPut("402", "Payment Required")
WebServer responseStatusCodes atPut("403", "Forbidden")
WebServer responseStatusCodes atPut("404", "Not Found")
WebServer responseStatusCodes atPut("405", "Method Not Allowed")
WebServer responseStatusCodes atPut("406", "Not Acceptable")
WebServer responseStatusCodes atPut("407", "Proxy Authentication Required")
WebServer responseStatusCodes atPut("408", "Request Time-out")
WebServer responseStatusCodes atPut("409", "Conflict")
WebServer responseStatusCodes atPut("410", "Gone")
WebServer responseStatusCodes atPut("411", "Length Required")
WebServer responseStatusCodes atPut("412", "Precondition Failed")
WebServer responseStatusCodes atPut("413", "Request Entity Too Large")
WebServer responseStatusCodes atPut("414", "Request-URI Too Large")
WebServer responseStatusCodes atPut("415", "Unsupported Media Type")
WebServer responseStatusCodes atPut("416", "Request range not satisfiable")
WebServer responseStatusCodes atPut("417", "Expectation Failed")
WebServer responseStatusCodes atPut("500", "Internal Server Error")
WebServer responseStatusCodes atPut("501", "Not Implemented")
WebServer responseStatusCodes atPut("502", "Bad Gateway")
WebServer responseStatusCodes atPut("503", "Service Unavailable")
WebServer responseStatusCodes atPut("504", "Gateway Time-out")
WebServer responseStatusCodes atPut("505", "HTTP Version not supported")
// End status codes

// Initialize the headers
WebServer init := method(
	self handlers := Map clone
)

WebServer defaultHandler := WebUrlHandler clone
WebServer errorHandler := WebErrorHandler clone

WebServer setDefaultHandler := method(handler,
	self defaultHandler := handler
)

WebServer setErrorHandler := method(handler,
	self errorHandler := handler
)

WebServer addHandler := method(path, handler,
	self handlers atPut(path, handler)
)

WebServer getHandlerFor := method(path,
	handlers at(path)
)

WebServer hasRequest := method(buf,
	buf containsSeq("\r\n\r\n")
)

// Main loop that processes request
WebServer processRequest := method(aSocket, aServer,
	request := WebRequest clone parseRequest(aSocket readBuffer)
	handler := getHandlerFor(request path)
	if(handler == nil, handler := self defaultHandler)

	e := try(handler handleRequest(request, aSocket))
	e catch("WebServer.ProtocolError",
		errorHandler handleError(request, aSocket,
			exceptionName, exceptionDescription)
	)

	ds := Date clone now asString("[%d/%b/%Y:%X %Z]")
	write(aSocket host, " - - ", ds, " \"", request reqMethod, " ",
		request path, " ", request httpVersion, "\" ", aSocket status, " ",
		aSocket bytesWritten, "\n")
	aSocket close
)

WebServer handleSocketFromServer := method(aSocket, aServer,
	// write("[New Request ", aSocket host, "]\n")
	sock := WebResponse clone setSocket(aSocket)

	while(aSocket isOpen,
		if(aSocket read,
			if(hasRequest(sock readBuffer),
				processRequest(sock, aServer)
				aSocket readBuffer empty
			)
		)
	)
	// write("[Closed ", aSocket host, "]\n")
)

//
// End of WebServer implementation
//

// ----------------------------------------------------------------------
//
// URL Handlers
//
// ----------------------------------------------------------------------

// The default handler provides functionality for sending responses, but
// just issues 404s

WebUrlHandler handleRequest := method(req, socket,
	raiseException("WebServer.ProtocolError.404",
		"No handler for " ..(req path))
)

// Error handler

WebErrorHandler handleRequest := method(req, socket,
	raiseException("WebServer.ProtocolError.405",
		"Error handlers don't provide content")
)

WebErrorHandler handleError := method(request, aSocket, eName, eDescription,
	status := eName substring(24)
	statusName := WebServer responseStatusCodes at(status)
	aSocket setStatus(status)
	// Send the error
	aSocket write(status, " - ", statusName, "\r\n")
	aSocket write(eDescription .. "\n")
)

// A happier handler

WebUrlTimeHandler handleRequest := method(req, socket,
	socket addHeader("Content-type", "text/plain")
	d = Date clone now
	ds = d asString("%Y/%m/%d %X")
	socket write("The current time is ", ds)
)

// Standard web handling

WebUrlStdHandler parseGet := method(req, socket,
	// write("Parsing GET from ", req queryString, "\n")
	CGI clone parseString(req queryString)
)

// Grab a chunk of the body
WebUrlStdHandler readFromBody := method(req, socket, length,
	while(socket readBuffer length < length,
		socket read)
	btmp := socket readBuffer fromTo(0, length - 1)
	btmp
)

WebUrlStdHandler parsePost := method(req, socket,
	// write("Parsing POST\n")
	formType := req headers at("Content-Type")
	if(formType != "application/x-www-form-urlencoded",
		raiseException("WebServer.ProtocolError.500",
			"Cannot handle form of type " .. formType))
	length := req headers at("Content-Length") asNumber
	btmp := readFromBody(req, socket, length)
	// write("Read length:  ", btmp length, "\n")
	s := String clone append(btmp asString)
	CGI clone parseString(s)
)

WebUrlStdHandler getParameters := method(req, socket,
	rv := nil
	m := req reqMethod
	if(m == "GET", rv = parseGet(req, socket))
	if(m == "POST", rv = parsePost(req, socket))
	rv
)

// Print out debug stuff

WebUrlDebugHandler handleRequest := method(req, socket,
	socket addHeader("Content-type", "text/plain")
	socket write("Method is ", req reqMethod, "\n")
	socket write("Path is ", req path, "\n")
	socket write("Query string is ", req queryString, "\n")
	cgi := getParameters(req, socket)
	if(cgi count > 0,
		cgi foreach(k, v, socket write("\t", k, " = ", v, "\n")))
	socket write("Headers:\n")
	req headers foreach(k, v, socket write("\t", k, " = ", v, "\n"))
)

