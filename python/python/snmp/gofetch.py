#!/usr/bin/env python
"""
Collect SNMP data regularly.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: gofetch.py,v 1.1 2002/03/28 07:09:27 dustin Exp $
"""

# Python's schedular
import sched
# Time, sleep, etc...
import time
# Thread support
import threading
# My kick-ass SNMP lib.  :)
import snmplib

class SNMPJob:
	"""An SNMP query job that needs to be performed."""

	def __init__(self, host, community, oid, freq):
		self.host=host
		self.community=community
		self.oid=oid
		self.frequency=freq

	def go(self):
		"""Perform this job."""
		s=snmplib.SnmpSession(self.host, self.community)
		rv=s.get(self.oid)
		print str(self.community) + "@" + str(self.host) \
			+ ":" + str(self.oid) + " => " + str(rv)

class StopRunning:
	"""Exception raised when the execution of the thread should cease."""
	pass

class SNMPCollector(threading.Thread):
	"""A class to sit around and collect data via SNMP."""

	def __init__(self):
		threading.Thread.__init__(self)
		self.setName("SNMPCollector")
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
		try:
			self.schedular.run()
		except StopRunning:
			print str(self) + " stopping."

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
		self.schedular.enter(0, 0, self.__stop, [])

	# This is just here to keep something in the job queue.  When the job
	# queue is empty, it stops looking.
	def __reschedule(self):
		self.schedular.enter(60, 100, self.__reschedule, [])

if __name__ == '__main__':
	sc=SNMPCollector()

	sc.addJob(SNMPJob('butterfly', 'public', 'ifInOctets.1', 5))
	sc.addJob(SNMPJob('butterfly', 'public', 'ifOutOctets.1', 5))
	sc.addJob(SNMPJob('juan', 'public', 'ifInOctets.1', 10))
	sc.addJob(SNMPJob('juan', 'public', 'ifOutOctets.1', 10))

	sc.start()

	time.sleep(10)

	print "Requesting a stop."
	sc.stop()
