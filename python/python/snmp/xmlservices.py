#!/usr/bin/env python
"""

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: xmlservices.py,v 1.1 2002/04/04 10:01:51 dustin Exp $
"""

from SimpleXMLRPCServer import SimpleXMLRPCServer
import threading

class Handler:
	"""This class handles all RPC requests."""

	def __init__(self, queue):
		"""Get a handler watching the given queue."""
		self.queue=queue

	def getQueueSize(self):
		"""Get the number of jobs in the queue."""
		return(len(self.queue.queue))

class Listener(threading.Thread):
	"""An XMLRPC listener to deal with queue manipulation."""

	def __init__(self, port, queue):
		"""Get a listener for a given port."""
		self.port=port
		self.queue=queue
		threading.Thread.__init__(self)
		self.setName("XMLRPC Listener:" + str(port))
		self.setDaemon(1)

	def run(self):
		"""Run forever on the configured port."""
		s=SimpleXMLRPCServer(("0.0.0.0", self.port))
		handler=Handler(self.queue)
		s.register_instance(handler)
		print "Starting XML RPC Handler."
		s.serve_forever()
