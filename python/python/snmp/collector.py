#!/usr/bin/env python
"""
Collect network data regularly.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: collector.py,v 1.3 2002/04/09 20:42:48 dustin Exp $
"""

# Python's scheduling stuff
import sched, time

# Error stuff
import traceback

# This is all the jobs stuff
import jobs

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

	def initXMLRPC(self, port):
		"""Initialize the XML RPC listener."""
		# Import here so we can deal with the case where something doesn't
		# work.
		import xmlservices
		self.listener=xmlservices.Listener(port, self.schedular)
		self.listener.start()

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
	import sys
	nc=NetworkCollector()

	if(len(sys.argv)>1):
		# Start RPC services.
		nc.initXMLRPC(int(sys.argv[1]))

	try:
		nc.addJob(jobs.VolatileSNMPJob('lamer', 'public', 'sysDescr.0', 60))
		nc.addJob(jobs.RRDSNMPJob('lamer', 'public',
			('ifInOctets.2', 'ifOutOctets.2'), 60, 'rrd/lamer_int.rrd'))
		nc.addJob(jobs.RRDSNMPJob('lamer', 'public',
			('ipInReceives.0', 'ipInDelivers.0'), 60, 'rrd/lamer_ip_in.rrd'))
		nc.addJob(jobs.RRDSNMPJob('lamer', 'public',
			('udpInDatagrams.0', 'udpOutDatagrams.0'), 60, 'rrd/lamer_udp.rrd'))
		nc.addJob(jobs.RRDSNMPJob('lamer', 'public',
			('tcpInSegs.0', 'tcpOutSegs.0'), 60, 'rrd/lamer_tcp.rrd'))
		nc.addJob(jobs.RRDSNMPJob('lamer', 'public',
			('ssCpuRawUser.0', 'ssCpuRawSystem.0',
			 'ssCpuRawIdle.0', 'ssCpuRawWait.0',
			 'ssCpuRawKernel.0'), 60, 'rrd/lamer_cpu.rrd'))

		# Add a job to watch for listening connections
		nc.addJob(jobs.SNMPWalkCountJob('lamer', 'public','tcpConnState',5,2))

		# Add a job to watch for the SMTP banner
		# nc.addJob(SMTPBannerJob('lamer', 5))
		nc.addJob(jobs.SMTPBannerJob('zuul', 60))
		nc.addJob(jobs.SMTPBannerJob('hazard', 60))
		nc.addJob(jobs.SMTPBannerJob('pagerdev', 60))
		nc.addJob(jobs.SMTPBannerJob('util1', 60))
		nc.addJob(jobs.SMTPBannerJob('util2', 60))
		nc.addJob(jobs.SMTPBannerJob('util3', 60))
		nc.addJob(jobs.SMTPBannerJob('doesnotexist', 60))
		nc.addJob(jobs.SMTPBannerJob('pix0', 60))
		nc.addJob(jobs.SMTPBannerJob('ld0', 60))

		nc.run()

	finally:
		print "Requesting a stop."
		c.stop()
