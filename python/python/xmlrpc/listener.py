#!/usr/bin/env python

from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler

# custom handler, deal with every type of request
class Handler(SimpleXMLRPCRequestHandler):
	def _dispatch(self, method, params):
		print "Call to method " + method + " args:  ", params
		return "Kill Whitey!"

# Create and start the server
SimpleXMLRPCServer(("0.0.0.0", 8000), Handler).serve_forever()
