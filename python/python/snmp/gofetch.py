#!/usr/bin/env python
"""
Collect SNMP data regularly.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: gofetch.py,v 1.4 2002/03/29 09:05:17 dustin Exp $
"""

# Python's scheduling stuff
import sched, time

# System stuff
import os, sys, atexit

# DBM
import anydbm

# Network stuff
import socket, select

# Error stuff
import traceback, errno, exceptions

# My kick-ass SNMP lib.  :)
import snmplib

# My rrdtool interface
import rrdpipe

# Threading support, if available
try:
	import threading
except ImportError, ie:
	sys.stderr.write(str(ie) + "\n")

######################################################################
# Storage classes
######################################################################

class Storage:
	"""Base class for data storage."""
	pass

class NonThreadSafeStorage(Storage):
	"""Base class for storage mechanisms that are not thread safe."""

	def __init__(self):
		try:
			self.mutex=threading.Lock()
		except NameError:
			# No threading support, go without
			self.mutex=None

	def lock(self):
		"""Lock mutex."""
		if self.mutex != None:
			self.mutex.acquire()

	def unlock(self):
		"""Unlock mutex."""
		if self.mutex != None:
			self.mutex.release()

class ThreadSafeStorage(Storage):
	"""Base class for storage mechanisms that are thread safe."""
	pass

class VolatileStorage(NonThreadSafeStorage):
	"""Storage for mechanism for volatile data sources."""

	db=None
	logfile=None

	def __init__(self):
		# Super constructor
		NonThreadSafeStorage.__init__(self)
		# Init the DB
		if VolatileStorage.db == None:
			VolatileStorage.db=anydbm.open('volatile.db', 'c')
			atexit.register(self.__exitHandler)
		# Init the log file
		if VolatileStorage.logfile == None:
			VolatileStorage.logfile = file('volatile.log', 'a')

	def __exitHandler(self):
		print "Exit handler closing DB."
		VolatileStorage.db.close()

	def recordState(self, key, value):
		"""Record the state of a given item, logging when it changes."""
		# Get the timestamp ASAP
		ts=time.time()
		# Aquire a lock
		try:
			self.lock()
			if VolatileStorage.db.has_key(key):
				oldvalue=VolatileStorage.db[key]
			else:
				oldvalue=''
			if str(value) != oldvalue:
				# If it changed, log it and record it
				VolatileStorage.logfile.write(str(ts) + '\t' \
					+ str(key) + '\t' + str(value) + '\n')
				VolatileStorage.logfile.flush()
				VolatileStorage.db[key]=str(value)
		finally:
			self.unlock()

class RRDStorage(NonThreadSafeStorage):
	"""Storage for data that will be make it into an rrd."""

	rrd=None

	def __init__(self):
		NonThreadSafeStorage.__init__(self)

		if RRDStorage.rrd == None:
			RRDStorage.rrd = rrdpipe.RRDPipe(
				'/usr/local/rrdtool-1.0.33/bin/rrdtool')

	def recordState(self, rrdfile, data, timestamp=None):
		"""Record data into the rrd.

		data may be a list, tuple, or dict object.  In the case of a dict
		object, all keys must be valid data stores in the rrd.

		If a timestamp isn't provided, it will default to the current time.
		"""
		# Get the timestamp ASAP
		ts=time.time()
		try:
			self.lock()

			cmd='update ' + rrdfile

			if timestamp == None:
				timestamp='N'

			if isinstance(data, dict):
				k=data.keys()
				# Get the template
				t=':'.join(k)
				# Get the values for the template
				v=':'.join(map(lambda x: str(data[x]), data.keys()))

				# append the template and the values
				cmd+=' -t ' + t + ' ' + timestamp + ':' + v
			elif isinstance(data, tuple) or isinstance(data, list):
				v=':'.join(map(lambda x: str(x), data))
				# append the values
				cmd+=' ' + timestamp + ':' + v
			else:
				raise TypeError(data)
			# Send the command
			try:
				# print "RRD command"  + cmd
				RRDStorage.rrd.sendCommand(cmd)
			except rrdpipe.RRDError, e:
				print e
		finally:
			self.unlock()

######################################################################
# End storage classes
######################################################################


######################################################################
# Job classes
######################################################################

class Job:
	"""Superclass of all jobs."""

	def __init__(self, freq):
		"""Get a job that repeats at the given frequency."""
		self.frequency=freq

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
			VolatileJob.db=VolatileStorage()

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

	def go(self):
		"""Perform this job."""
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.get(self.oid)
		print str(self.community) + "@" + str(self.host) \
			+ ":" + str(self.oid) + " => " + str(rv)

class VolatileSNMPJob(VolatileJob, SNMPJob):
	"""An SNMP job that records its state in the volatile DB."""

	def __init__(self, host, community, oid, freq):
		# Call the super constructors
		VolatileJob.__init__(self, freq)
		SNMPJob.__init__(self, host, community, oid, freq)

	def go(self):
		"""Get and record the data."""
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.get(self.oid)
		k='snmp:' + self.host + ':' + self.oid
		self.recordState(k, rv)

class SNMPWalkCountJob(VolatileSNMPJob):
	"""An SNMP job that walks a tree and records the number of things it saw."""

	def __init__(self, host, community, oid, freq, match=None):
		# Call the super constructors
		VolatileSNMPJob.__init__(self, host, community, oid, freq)
		self.match=match

	def go(self):
		"""Get and record the data."""
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.countBranch(self.oid, self.match)
		k='snmpwalk:' + self.host + ':' + self.oid
		self.recordState(k, rv)

class RRDJob(Job):
	"""Base class for jobs that record data via RRD."""

	rrd=None

	def __init__(self, freq):
		Job.__init__(self, freq)
		if RRDJob.rrd == None:
			RRDJob.rrd=RRDStorage()

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
		s=snmplib.SnmpSession(self.host, self.community)
		oids, rvs=s.multiGet(self.oid)
		self.recordState(self.rrdfile, rvs)

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

		k='smtp:' + self.host + ':' + `self.port`
		self.recordState(k, banner.rstrip())

######################################################################
# End job classes
######################################################################

class StopRunning:
	"""Exception raised when the execution of the thread should cease."""
	pass

class NetworkCollector:
	"""A class to sit around and collect data via SNMP."""

	def __init__(self):
		self.schedular=sched.scheduler(time.time, time.sleep)
		self.__reschedule()

	def addJob(self, job):
		"""Add an SNMPjob to the queue.

		job is an SNMPJob object
		"""
		# Schedule the job to run immediately, it will be rescheduled to
		# run at its proper frequency in the future.
		self.schedular.enter(0, 5, self.__runJob, [job])

	def run(self):
		"""Loop forever, processing jobs."""
		keepgoing=1
		while keepgoing:
			try:
				print "Entering run loop."
				self.schedular.run()
			except (StopRunning, KeyboardInterrupt):
				print str(self) + " stopping."
				keepgoing=None
			except:
				traceback.print_exc()

	# This runs a job and reschedules it
	def __runJob(self, job):
		# Run it
		job.go()
		# Reschedule
		self.schedular.enter(job.frequency, 5, self.__runJob, [job])

	# Job performed when it's time to stop running
	def __stop(self):
		raise StopRunning

	def stop(self):
		"""Tell the schedular to stop whenever it can."""
		print "Scheduling a stop."
		self.schedular.enter(0, 0, self.__stop, [])

	# This is just here to keep something in the job queue.  When the job
	# queue is empty, it stops looking.
	def __reschedule(self):
		self.schedular.enter(60, 100, self.__reschedule, [])

######################################################################
# Main loop below
######################################################################

if __name__ == '__main__':
	nc=NetworkCollector()

	try:
		nc.addJob(VolatileSNMPJob('lamer', 'public', 'sysDescr.0', 5))
		nc.addJob(RRDSNMPJob('lamer', 'public',
			('ifInOctets.2', 'ifOutOctets.2'), 5, 'test.rrd'))

		# Add a job to watch for listening connections
		nc.addJob(SNMPWalkCountJob('lamer', 'public', 'tcpConnState', 5, 2))

		# Add a job to watch for the SMTP banner
		# nc.addJob(SMTPBannerJob('lamer', 5))
		nc.addJob(SMTPBannerJob('zuul', 60))
		nc.addJob(SMTPBannerJob('hazard', 60))
		nc.addJob(SMTPBannerJob('pagerdev', 60))
		nc.addJob(SMTPBannerJob('util1', 60))
		nc.addJob(SMTPBannerJob('util2', 60))
		nc.addJob(SMTPBannerJob('util3', 60))
		nc.addJob(SMTPBannerJob('doesnotexist', 60))
		nc.addJob(SMTPBannerJob('pix0', 60))
		nc.addJob(SMTPBannerJob('ld0', 60))

		nc.run()

		time.sleep(300)

	finally:
		print "Requesting a stop."
		nc.stop()
