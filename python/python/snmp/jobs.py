#!/usr/bin/env python
"""
Collect SNMP data regularly.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: jobs.py,v 1.1 2002/04/04 20:00:46 dustin Exp $
"""

# time is important
import time

# Network stuff
import socket, select

# Error stuff
import errno, exceptions

# My kick-ass SNMP lib.  :)
import snmplib

# Data storage classes
import storage

######################################################################
# Job classes
######################################################################

class Job:
	"""Superclass of all jobs."""

	db=None

	def __init__(self, freq):
		"""Get a job that repeats at the given frequency."""
		self.frequency=freq
		if Job.db == None:
			Job.db=storage.DBMStorage('hostmarks.db')

	def mark(self, host):
		"""Mark activity on a host."""
		Job.db[host]=str(time.time())

	def go(self):
		"""This method is called when it's time to perform the job."""
		raise exceptions.NotImplementedError

class VolatileJob(Job):
	"""Jobs whose values should not change."""

	db=None

	def __init__(self, freq):
		# Initialize the superclass
		Job.__init__(self, freq)
		if VolatileJob.db==None:
			VolatileJob.db=storage.VolatileStorage()

	def recordState(self, key, state):
		"""Record the current state of the task."""
		VolatileJob.db.recordState(key, state)

class SNMPJob(Job):
	"""An SNMP query job that needs to be performed."""

	def __init__(self, host, community, oid, freq):
		Job.__init__(self, freq)
		self.host=host
		self.community=community
		self.oid=oid

class VolatileSNMPJob(VolatileJob, SNMPJob):
	"""An SNMP job that records its state in the volatile DB."""

	def __init__(self, host, community, oid, freq):
		# Call the super constructors
		VolatileJob.__init__(self, freq)
		SNMPJob.__init__(self, host, community, oid, freq)

	def go(self):
		"""Get and record the data."""
		# Get the data and record the state
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.get(self.oid)
		k='snmp:' + self.host + ':' + self.oid
		self.recordState(k, rv)
		# Mark it
		self.mark(self.host)

class SNMPWalkCountJob(VolatileSNMPJob):
	"""An SNMP job that walks a tree and records the number of things it saw."""

	def __init__(self, host, community, oid, freq, match=None):
		# Call the super constructors
		VolatileSNMPJob.__init__(self, host, community, oid, freq)
		self.match=match

	def go(self):
		"""Get and record the data."""
		# Get the data and record it
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.countBranch(self.oid, self.match)
		k='snmpwalk:' + self.host + ':' + self.oid
		self.recordState(k, rv)
		# Mark it
		self.mark(self.host)

class RRDJob(Job):
	"""Base class for jobs that record data via RRD."""

	rrd=None

	def __init__(self, freq):
		Job.__init__(self, freq)
		if RRDJob.rrd == None:
			RRDJob.rrd=storage.RRDStorage()

	def recordState(self, rrdfile, data, timestamp=None):
		"""Record the current state."""
		RRDJob.rrd.recordState(rrdfile, data, timestamp)

class RRDSNMPJob(RRDJob, SNMPJob):
	"""A job that collects data from snmp and stores it in an rrd.

	More than one oid may be collected at a time.
	"""

	def __init__(self, host, community, oids, freq, rrdfile):
		RRDJob.__init__(self, freq)
		SNMPJob.__init__(self, host, community, oids, freq)
		self.rrdfile=rrdfile

	def go(self):
		"""Get and record the data."""
		# Get the data and record it
		s=snmplib.SnmpSession(self.host, self.community)
		oids, rvs=s.multiGet(self.oid)
		self.recordState(self.rrdfile, rvs)
		# Mark it
		self.mark(self.host)

class SMTPBannerJob(VolatileJob):
	"""A volatile job to monitor SMTP banners."""

	def __init__(self, host, freq, port=25):
		VolatileJob.__init__(self, freq)
		self.host=host
		self.port=port

	def go(self):
		print "Checking SMTP banner on " + self.host
		sock=None
		for res in socket.getaddrinfo(self.host, self.port,
										0, socket.SOCK_STREAM):

			af, socktype, proto, canonname, sa = res
			try:
				sock=socket.socket(af, socktype, proto)
				# connect with a timeout
				sock.setblocking(0)
				try:
					sock.connect(sa)
				except socket.error, e:
					# Check for error in progress (expected)
					if e[0] != errno.EINPROGRESS:
						raise e
				# Ten second timeout
				rl, wl, xl=select.select([sock.fileno()], [], [], 10)
				if sock.fileno() not in rl:
					print "Timed out!"
					raise socket.error("Timeout")
				sock.setblocking(1)
			except socket.error, se:
				if sock:
					sock.close()
				sock=None
				# Try again
				continue
			# If we got here, we're connected
			break
		# Raise the socket error from above if we still don't have a socket
		if not sock:
			raise socket.error(se)
		# XXX Something could block for a while here.
		f=sock.makefile('r')
		banner=f.readline()
		f.close()
		sock.close()
		# Mark it
		self.mark(self.host)

		k='smtp:' + self.host + ':' + `self.port`
		self.recordState(k, banner.rstrip())

######################################################################
# End job classes
######################################################################
