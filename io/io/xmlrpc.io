#!/usr/bin/env ioServer

// Object definitions

// The xml proxy client
XmlRpcProxy = Object clone
// The xmlrpc server
XmlRpcServer = Object clone

XmlRpcSerializer = Object clone
XmlRpcDeserializer = Object clone
XmlRpcResponseDecoder = Object clone

// ----------------------------------------------------------------------
// Serializer implementation
// ----------------------------------------------------------------------

XmlRpcSerializer DATEFORMAT = "%Y%m%dT%H:%M:%S"

XmlRpcSerializer normalize = method(s,
	s
)

XmlRpcSerializer serializeString = method(v,
	"<string>" .. normalize(v) .. "</string>"
)

XmlRpcSerializer serializeInt = method(v,
	"<int>" ..(v asString)..("</int>")
)

XmlRpcSerializer serializeDouble = method(v,
	"<double>" ..(v asString)..("</double>")
)

XmlRpcSerializer serializeNumber = method(v,
	rv = Nil
	if(v == v floor,
		rv = serializeInt(v),
		rv = serializeDouble(v))
	rv
)

XmlRpcSerializer serializeBoolean = method(v,
	ev = "0"
	if(v == 1, ev = "1")
	"<boolean>" .. ev .."</boolean>"
)

XmlRpcSerializer serializeDate = method(v,
	"<dateTime.iso8601>" ..(v asString(DATEFORMAT)) ..("</dateTime.iso8601>")
)

XmlRpcSerializer serializeList = method(l,
	b = Buffer clone
	b append("<array><data>")
	l doBlock(block(v,
		b append("<value>")
		b append(serialize(v))
		b append("</value>")))
	b append("</data></array>")
	b asString
)

XmlRpcSerializer serializeMap = method(l,
	b = Buffer clone
	b append("<struct>")
	l doBlock(block(k, v,
		b append("<member>")
		b append("<name>")
		b append(normalize(k))
		b append("</name>")
		b append("<value>")
		b append(serialize(v))
		b append("</value>")))
	b append("</struct>")
	b asString
)

XmlRpcSerializer serialize = method(o,
	sn = "serialize" ..(o type)
	// Validate there's an encoder for this thing
	if(XmlRpcSerializer hasSlot(sn) == Nil,
		raiseException("XmlRpc.UnhandledObject", o type))
	// get the encoder
	m = XmlRpcSerializer getSlot(sn)
	// Invoke the method
	m(o)
)

// ----------------------------------------------------------------------
// Structure deserializer
// ----------------------------------------------------------------------

XmlRpcDeserializer handlers = Map clone

// Deserialize parsed XML
XmlRpcDeserializer deserializeXML = method(x,
	// write("Deserializing ", x type, ": ", x, "\n")
	rv = x
	if(x type != "String",
		m = handlers at(x name)
		rv = m(x))
	rv
)

// Deserialize what *may* be parsed XML.  If it's a string, parse it first
XmlRpcDeserializer deserialize = method(x,
	// Make sure it's an XML object
	if(x type == "String", x = x asXML)
	deserializeXML(x)
)

XmlRpcDeserializer nopDeserializer=method(v,
	deserializeXML(v subitems at(0))
)

XmlRpcDeserializer numberDeserializer=method(v,
	v subitems at(0) asNumber
)

XmlRpcDeserializer boolDeserializer=method(v,
	v subitems at(0) asNumber != 0
)

XmlRpcDeserializer stringDeserializer=method(v,
	v subitems at(0)
)

XmlRpcDeserializer dateDeserializer=method(v,
	s = v subitems at(0)
	write("Deserializing date:  ", s, " using format ",
		XmlRpcSerializer DATEFORMAT, "\n")
	Date clone fromString(s, XmlRpcSerializer DATEFORMAT)
)

XmlRpcDeserializer arrayDeserializer=method(v,
	data = v subitems at(0)
	// Validation
	if(data name != "data",
		raiseException("XmlRpc.ParseException", "array did not contain data"))
	rv = List clone
	data subitems doBlock(block(i,
		rv add(deserializeXML(i))))
	rv
)

XmlRpcDeserializer structDeserializer=method(v,
	rv = Map clone
	v subitems doBlock(block(i,
		if(i name != "member",
			raiseException("XmlRpc.ParseException",
				"expected struct member, was " ..(i name)))
		// This looks weird, but it's <name>something</name> and
		// <value>something</value> so I need to grab the field, then its
		// contents.
		n = deserializeXML(i subitems at(0) subitems at(0))
		v = deserializeXML(i subitems at(1) subitems at(0))
		rv atPut(n, v)
	))
	rv
)

XmlRpcDeserializer handlers atPut("",
	XmlRpcDeserializer getSlot("nopDeserializer"))
XmlRpcDeserializer handlers atPut("?xml",
	XmlRpcDeserializer getSlot("nopDeserializer"))
XmlRpcDeserializer handlers atPut("value",
	XmlRpcDeserializer getSlot("nopDeserializer"))
XmlRpcDeserializer handlers atPut("param",
	XmlRpcDeserializer getSlot("nopDeserializer"))
XmlRpcDeserializer handlers atPut("params",
	XmlRpcDeserializer getSlot("nopDeserializer"))
XmlRpcDeserializer handlers atPut("i4",
	XmlRpcDeserializer getSlot("numberDeserializer"))
XmlRpcDeserializer handlers atPut("int",
	XmlRpcDeserializer getSlot("numberDeserializer"))
XmlRpcDeserializer handlers atPut("double",
	XmlRpcDeserializer getSlot("numberDeserializer"))
XmlRpcDeserializer handlers atPut("boolean",
	XmlRpcDeserializer getSlot("boolDeserializer"))
XmlRpcDeserializer handlers atPut("string",
	XmlRpcDeserializer getSlot("stringDeserializer"))
XmlRpcDeserializer handlers atPut("datetime.iso8601",
	XmlRpcDeserializer getSlot("dateDeserializer"))
XmlRpcDeserializer handlers atPut("array",
	XmlRpcDeserializer getSlot("arrayDeserializer"))
XmlRpcDeserializer handlers atPut("struct",
	XmlRpcDeserializer getSlot("structDeserializer"))

// ----------------------------------------------------------------------
// Response decoder
// ----------------------------------------------------------------------

XmlRpcResponseDecoder processFault = method(f,
	m = XmlRpcDeserializer deserialize(f subitems at(0))
	// First, we need to do a pass where we lowercase keys
	tmp = Map clone
	m foreach(k, v, tmp atPut(k lower, v))
	fc = tmp at("faultcode")
	fs = tmp at("faultstring")
	raiseException("XmlRpc.Fault." .. (fc asString), fs)
)

XmlRpcResponseDecoder decode = method(xml,
	x = xml asXML
	while(x name lower != "methodresponse",
		x = x subitems at(0))
	// Figure out if this is a fault or a real response
	f = x subitems at(0)
	if(f name lower == "fault",
		processFault(f))
	rv = XmlRpcDeserializer deserializeXML(f)
)

// ----------------------------------------------------------------------
// Proxy client implementation
// ----------------------------------------------------------------------

// proxy
XmlRpcProxy setURL = method(u,
	self url = u
	u
)

// Perform the required POST
XmlRpcProxy sendRequest = method(xml,
	h = Host clone setName(url host) address
	// s = Socket clone setHost(h) setPort(80) connect
	s = Socket clone setHost(h) setPort(8456) connect
	if(s error, raiseException("XmlRpc.ConnectError", s error))
	s write("POST " ..(url path) ..(" HTTP/1.0\r\n"))
	s write("User-Agent: Dustin's Io XMLRPC Client\r\n")
	s write("Host: " ..(url host) ..("\r\n"))
	s write("Content-Type: text/xml\r\n")
	s write("Content-Length: " ..(xml length) ..("\r\n"))
	s write("Connection: close\r\n")
	s write("\r\n")
	s write(xml)

	// Read the result
	while(s read, Nop)

	s close

	// Extract the XML from the response
	buf = s readBuffer
	xml = buf fromTo(buf find("\r\n\r\n") + 4, buf length - 1)

	// write("Read the following:  ", xml, "\n")

	// Find the method response
	response = XmlRpcResponseDecoder decode(xml)
)

// Invoke an XML-RPC method, return object response
XmlRpcProxy invoke = method(mn, params,
	b = Buffer clone
	b append("<?xml version=\"1.0\" ?>\r\n")
	b append("<methodCall>")
	b append("<methodName>")
	b append(mn)
	b append("</methodName>")
	if(params type == "List",
		b append("<params>")
		params doBlock(block(v,
			b append("<param><value>")
			b append(XmlRpcSerializer serialize(v))
			b append("</value></param>")
		))
		b append("</params>")
	)
	b append("</methodCall>")

	// write("Posting ", b, " to ", url url, "\n")
	rv = sendRequest(b asString)
)

// Automatic invocation
XmlRpcProxy forward = method(
	methodName = thisMessage name
	args = thisMessage argsEvaluatedIn(sender)
	if(args count == 0, args = Nil)
	invoke(methodName, args)
)

// ----------------------------------------------------------------------
// XMLRPC server implementation
// ----------------------------------------------------------------------

XmlRpcServer handlers = Nil

XmlRpcServer init = method(
	self handlers = Map clone
)

// Add a handler
XmlRpcServer addHandler = method(m, h,
	handlers atPut(m, h)
)

XmlRpcServer findItem = method(x, v,
	rv = Nil
	if(x type == "SGMLTag",
		if(x name lower == v,
			rv = x,
			x subitems foreach(i,
				rv = findItem(i, v)
				if(rv != Nil, return rv))))
	rv
)

// Internal method to deal with xml in and out
XmlRpcServer rawInvoke = method(xml,
	x = xml asXML
	x = findItem(x, "methodcall")
	// Alright, let's build a map of the pieces
	tmp = Map clone
	x subitems foreach(i,
		if(i type == "SGMLTag",
			tmp atPut(i name lower, i))
	)
	methodName = tmp at("methodname") subitems at(0)
	if(methodName == Nil,
		raiseException("XmlRpc.ParseException", "no method name"))
	params = list()
	if(tmp at("params"),
		params=list(XmlRpcDeserializer deserializeXML(tmp at("params"))))

	mparts = methodName splitAt(methodName find("."))
	moduleName = mparts at(0)
	methodName = mparts at(1) substring(1)

	h = handlers at(moduleName)
	if(h == Nil,
		raiseException("XmlRpc.NoSuchMethod", "no handler for " .. moduleName))

	XmlRpcSerializer serialize(h performWithArgList(methodName, params))
)

XmlRpcServer responseSuccessful = method(x,
	b = Buffer clone
	b append("<params><param>")
	b append(x)
	b append("</param></params>")
	b asString
)

XmlRpcServer responseFault = method(exceptionName, exceptionDescription,
	m = Map clone
	m atPut("faultCode", 1)
	m atPut("faultString", exceptionName .. ": " ..  exceptionDescription)
	b = Buffer clone
	b append("<fault>")
	b append(XmlRpcSerializer serialize(m))
	b append("</fault>")
	b asString
)

// This is the method that will be called with the XML
XmlRpcServer invokeXML = method(xml,
	b = Buffer clone
	b append("<?xml version=\"1.0\"?>")
	b append("<methodResponse>")
	catchException("X",
		b append(responseSuccessful(rawInvoke(xml))),
		b append(responseFault(exceptionName, exceptionDescription)))

	b append("</methodResponse>")
)
