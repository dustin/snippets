#!/usr/bin/env python
"""
Data storage classes for collectors.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: storage.py,v 1.3 2002/05/01 20:33:45 dustin Exp $
arch-tag: A5E9D526-DAF1-44E5-8FBA-70F3E714F800
"""

# Time
import time

# System stuff
import sys, atexit

# DBM
import anydbm

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

	bigfatlock=None

	def __init__(self):
		try:
			if NonThreadSafeStorage.bigfatlock==None:
				NonThreadSafeStorage.bigfatlock=threading.Lock()
		except NameError, e:
			# No threading support, go without
			NonThreadSafeStorage.bigfatlock=None

	def lock(self):
		"""Lock mutex."""
		if NonThreadSafeStorage.bigfatlock != None:
			NonThreadSafeStorage.bigfatlock.acquire()

	def unlock(self):
		"""Unlock mutex."""
		if NonThreadSafeStorage.bigfatlock != None:
			NonThreadSafeStorage.bigfatlock.release()

class ThreadSafeStorage(Storage):
	"""Base class for storage mechanisms that are thread safe."""
	pass

class DBMStorage(NonThreadSafeStorage):
	"""Storage mechanism for any DBM data."""

	dbs={}
	initialized=None

	def __init__(self, filename):
		# Super constructor
		NonThreadSafeStorage.__init__(self)
		self.filename=filename
		# Init the DB
		if not DBMStorage.dbs.has_key(self.filename):
			DBMStorage.dbs[self.filename]=anydbm.open(self.filename, 'c')
		# Register the handler if it's not already registered
		if not DBMStorage.initialized:
			atexit.register(self.__exitHandler)
			DBMStorage.initialized=1

	def __exitHandler(self):
		print "Exit handler closing DB."
		for v in DBMStorage.dbs.itervalues():
			print "Closing " + str(v)
			v.close()

	def __getitem__(self, which):
		"""Get the value of a particular key from the underlying DBM."""
		return(DBMStorage.dbs[self.filename][which])

	def __setitem__(self, which, value):
		"""Set the value of a particular key from the underlying DBM."""
		DBMStorage.dbs[self.filename][which]=value

	def has_key(self, which):
		"""Return true of the underlying DBM has the given key."""
		return(DBMStorage.dbs[self.filename].has_key(which))

class VolatileStorage(DBMStorage):
	"""Storage for mechanism for volatile data sources."""

	logfile=None

	def __init__(self):
		"""Initialize DBMStorage with the volatile db"""
		DBMStorage.__init__(self, "volatile.db");

		# Init the log file
		if VolatileStorage.logfile == None:
			VolatileStorage.logfile = file('volatile.log', 'a')

	def recordState(self, key, value):
		"""Record the state of a given item, logging when it changes."""
		# Get the timestamp ASAP
		ts=time.time()
		# Aquire a lock
		try:
			self.lock()
			if self.has_key(key):
				oldvalue=self[key]
			else:
				oldvalue=''
			if str(value) != oldvalue:
				# If it changed, log it and record it
				VolatileStorage.logfile.write(str(ts) + '\t' \
					+ str(key) + '\t' + str(value) + '\n')
				VolatileStorage.logfile.flush()
				self[key]=str(value)
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
		self.performQuery(cmd)

	def performQuery(self, cmd):
		rv=None
		try:
			self.lock()
			try:
				rv=RRDStorage.rrd.sendCommand(cmd)
			except rrdpipe.RRDError, e:
				print e, "query: ", cmd
		finally:
			self.unlock()
		return rv

	def fetch(self, rrdfile, cf='AVERAGE', res=None, start=None, end=None):
		"""Fetch data out of the underlying rrd file."""
		rv=None
		try:
			self.lock()
			rv=RRDStorage.rrd.fetch(rrdfile, cf, res, start, end)
		finally:
			self.unlock()
		return rv


######################################################################
# End storage classes
######################################################################
