#!/usr/bin/env python
"""
Collect SNMP data regularly.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: jobs.py,v 1.5 2002/05/01 20:33:43 dustin Exp $
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
	prefix=None

	def __init__(self, descriptor, freq):
		"""Get a job that repeats at the given frequency.

		The descriptor argument provides a unique ID for this job based on
		what it does.
		"""
		self.descriptor=descriptor
		self.frequency=freq
		if Job.db == None:
			Job.db=storage.DBMStorage('hostmarks.db')
		if self.prefix==None:
			self.prefix='unknown'

	def getDescriptor(self):
		"""Get the descriptor this job uses."""
		return ':'.join((self.prefix, self.descriptor))

	def mark(self):
		"""Mark activity on a host."""
		Job.db[self.getDescriptor()]=str(time.time())

	def go(self):
		"""This method is called when it's time to perform the job."""
		raise exceptions.NotImplementedError

class VolatileJob(Job):
	"""Jobs whose values should not change."""

	db=None

	def __init__(self, descriptor, freq):
		# Initialize the superclass
		Job.__init__(self, descriptor, freq)
		if VolatileJob.db==None:
			VolatileJob.db=storage.VolatileStorage()
		self.prefix='volatile'

	def recordState(self, state):
		"""Record the current state of the task."""
		VolatileJob.db.recordState(self.getDescriptor(), state)

class SNMPJob(Job):
	"""An SNMP query job that needs to be performed."""

	def __init__(self, host, community, oid, freq, jobtype='snmp'):
		Job.__init__(self, ':'.join((jobtype, host, community, str(oid))), freq)
		self.host=host
		self.community=community
		self.oid=oid

class VolatileSNMPJob(VolatileJob, SNMPJob):
	"""An SNMP job that records its state in the volatile DB."""

	def __init__(self, host, community, oid, freq, jobtype='snmp'):
		# Call the super constructors
		# VolatileJob's constructor is called with a descriptor of XXXX
		# because SNMPJob should fill it it with the correct value.
		VolatileJob.__init__(self, 'XXXX', freq)
		SNMPJob.__init__(self, host, community, oid, freq, jobtype)

	def go(self):
		"""Get and record the data."""
		# Get the data and record the state
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.get(self.oid)
		self.recordState(rv)
		# Mark it
		self.mark()

class SNMPWalkCountJob(VolatileSNMPJob):
	"""An SNMP job that walks a tree and records the number of things it saw."""

	def __init__(self, host, community, oid, freq, match=None):
		# Call the super constructors
		VolatileSNMPJob.__init__(self, host, community, oid, freq, 'snmpwalk')
		self.match=match

	def go(self):
		"""Get and record the data."""
		# Get the data and record it
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.countBranch(self.oid, self.match)
		self.recordState(rv)
		# Mark it
		self.mark()

class RRDJob(Job):
	"""Base class for jobs that record data via RRD."""

	rrd=None

	def __init__(self, rrdfile, descriptor, freq):
		"""Get an RRDJob referencing the given rrdfile, using the given
		descriptor, and repeating at the specified frequency."""
		Job.__init__(self, descriptor, freq)
		if RRDJob.rrd == None:
			RRDJob.rrd=storage.RRDStorage()
		self.file=rrdfile
		self.prefix='performance'

	def getFilename(self):
		"""Get the name of the rrd file this job manipulates."""
		return self.file

	def recordState(self, data, timestamp=None):
		"""Record the current state."""
		RRDJob.rrd.recordState(self.file, data, timestamp)

	def getNames(self):
		"""Get the column names for this RRD."""
		raise exceptions.NotImplementedError

class RRDSNMPJob(RRDJob, SNMPJob):
	"""A job that collects data from snmp and stores it in an rrd.

	More than one oid may be collected at a time.
	"""

	def __init__(self, host, community, oids, freq, rrdfile):
		# RRDJob's constructor is called with a descriptor of XXXX because
		# SNMPJob should fill it it with the correct value.
		RRDJob.__init__(self, rrdfile, 'XXXX', freq)
		SNMPJob.__init__(self, host, community, oids, freq)

	def getNames(self):
		"""Get the SNMP variables that are being watched."""
		rv=self.oid
		if not isinstance(self.oid, list):
			rv=list(rv)
		assert(isinstance(rv, list))
		return(rv)

	def go(self):
		"""Get and record the data."""
		# Get the data and record it
		s=snmplib.SnmpSession(self.host, self.community)
		oids, rvs=s.multiGet(self.oid)
		self.recordState(rvs)
		# Mark it
		self.mark()

class SMTPBannerJob(VolatileJob):
	"""A volatile job to monitor SMTP banners."""

	def __init__(self, host, freq, port=25):
		VolatileJob.__init__(self, ':'.join(('smtp', host, `port`)), freq)
		self.host=host
		self.port=port

	def go(self):
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
					print "Timed out while looking for " \
						+ self.host + ":" + `self.port`
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
		self.mark()

		self.recordState(banner.rstrip())

class UnsupportedJobException(exceptions.Exception):
	"""Thrown when createJob is called with an unsupported job type."""
	pass

def createJob(jobtype, args):
	"""Create a job from an argument list that describes the job.

	Currently, the following job types are supported:

	 * VolatileSNMPJob (host, community, variable, frequency)
	 * RRDSNMPJob (host, community, variable(s), frequency, rrdfile)
	"""

	rv=None

	if jobtype == 'VolatileSNMPJob':
		rv=apply(VolatileSNMPJob, args)
	elif jobtype == 'RRDSNMPJob':
		rv=apply(RRDSNMPJob, args)
	else:
		raise UnsupportedJobException(jobtype)

	return rv

######################################################################
# End job classes
######################################################################
