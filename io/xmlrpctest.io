#!/usr/bin/env ioServer

// load the xmlrpc implementation
doFile("xmlrpc.io")

// Testing

XmlRpcTest = Object clone

XmlRpcTest printResult = method(rv,
	write("rv is ", rv type, ":  ", rv, "\n")
	if(rv type == "Map", rv foreach(k, v, write("\t", k, " = ", v, "\n")))
)

XmlRpcSerializerTest = XmlRpcTest clone

XmlRpcSerializerTest main = method(
	xs = XmlRpcSerializer clone
	
	write("String ", xs serialize("Testing..."), "\n")
	write("Int ", xs serialize(3), "\n")
	write("Double ", xs serialize(3.141592653589793262), "\n")
	write("Boolean (true) ", xs serialize(1), "\n")
	write("Boolean (false) ", xs serialize(0), "\n")
	write("Date ", xs serialize(Date clone now), "\n")
	write("List ", xs serialize(list(1, 2, 3)), "\n")
	m = Map clone
	m atPut("skey", "val1")
	m atPut("ikey", 2)
	m atPut("fkey", 1.71)
	m atPut("dkey", Date clone now)
	m atPut("lkey", list(5, 6, 7))
	write("Map ", xs serialize(m), "\n")
	
	write("\n")
)

XmlRpcDeserializerTest = XmlRpcTest clone

XmlRpcDeserializerTest main = method(
	s = """<?xml version="1.0" encoding="ISO-8859-1"?><methodResponse><params><param><value><double>20.02</double></value></param></params></methodResponse>"""
	/*
	// List
	s = """<?xml version="1.0" encoding="ISO-8859-1"?><array><data><value><string>19980717T14:08:55</string></value><value><i4>3</i4></value></data></array>>"""
	*/
	/*
	// Struct
	s = """<struct><member><name>an int</name><value><i4>34</i4></value></member><member><name>a string</name><value>wooo</value></member></struct>>"""
	*/
	// rv = XmlRpcDeserializer deserialize(s)
	rv = XmlRpcResponseDecoder decode(s)
	printResult(rv)
)

XmlRpcTest = XmlRpcTest clone

XmlRpcTest main = method(
	DNSResolver addServerAddress("192.168.1.40")

	u = URL clone setURL("http://bleu.west.spy.net/servlet/net.spy.rpc.XMLRPC")
	xp = XmlRpcProxy clone
	xp setURL(u)

	// xp zipcodes.lookupZip(95051)
	rv = xp therm.listTherms
	printResult(rv)
	rv = xp therm.getTemperatures
	printResult(rv)
	rv = xp therm.getTemperature("bedroom")
	printResult(rv)
)

// XmlRpcSerializerTest main
// XmlRpcDeserializerTest main

XmlRpcTest main
