#!/usr/bin/env python
"""

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: xmlservices.py,v 1.6 2002/05/02 19:09:43 dustin Exp $
"""

from SimpleXMLRPCServer import SimpleXMLRPCServer
import threading
import time

import jobs
import storage

class Handler:
	"""This class handles all RPC requests."""

	def __init__(self, queue):
		"""Get a handler watching the given queue."""
		self.queue=queue

	def getQueueSize(self):
		"""Get the number of jobs in the queue."""
		return(len(self.queue.schedular.queue))

	def emptyJobQueue(self):
		"""Empty the job queue."""
		self.queue.schedular.queue.clear()

	def listDescriptors(self):
		"""Get a list of all job descriptors in the queue."""
		rv=[]
		for item in self.queue.schedular.queue:
			print "Looking at " + `item`
			try:
				j=item[3][0]
				if isinstance(j, jobs.Job):
					print "Adding " + `j`
					rv.append(j.getDescriptor())
			except IndexError, e:
				print `e`
		return rv

	def __findJob(self, descriptor):
		rv=None
		for item in self.queue.schedular.queue:
			try:
				j=item[3][0]
				if isinstance(j, jobs.Job):
					if j.getDescriptor() == descriptor:
						rv=j
			except IndexError, e:
				print `e`
		if rv==None:
			raise "Couldn't find matching job."
		return rv

	def queryJob(self, descriptor, start=None, end=None):
		"""Query a job for performance data."""
		rv={}

		# First, find the job
		j=self.__findJob(descriptor)
		if not isinstance(j, jobs.RRDJob):
			raise "Can't do a performance query on a non-RRD job."
		# Get the rrd filename
		rrdfile=j.getFilename()

		# Grab an RRD storage object
		rstore=storage.RRDStorage()
		# Get the specified performance data
		perfdata=rstore.fetch(rrdfile, start=start, end=end)
		perfval=[]
		# Strip out any results with null columns
		for row in perfdata:
			save=1
			for col in row[1]:
				if col == None:
					save=None
			if save:
				perfval.append(row)
		rv['performance']=perfval
		rv['names']=j.getNames()

		return rv

	def queryVolatile(self, descriptor, start=None, end=None):
		"""Query a volatile job for changes."""
		rv=[]

		if start==None:
			start=0
		if end==None:
			end=time.time()

		logfile=file("volatile.log")
		line=logfile.readline()
		while line!='':
			parts=line.rstrip().split('\t')
			# If this is a match, store it
			if parts[1]==descriptor \
				and float(parts[0]) >= start \
				and float(parts[0]) <= end:
				rv.append( (float(parts[0]), parts[2]))
			line=logfile.readline()
		logfile.close()
		return rv

	def createJob(self, type, args):
		"""Create a new job using jobs.createJob and add it to the queue."""

		rv=None

		j=jobs.createJob(type, args)
		rv=j.getDescriptor()

		self.queue.addJob(j)

		return rv

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
