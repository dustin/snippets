#!/usr/bin/env python
# arch-tag: F790C1F1-9E89-4CEC-9709-F8A2D661214B

import sched
import time
import threading
import exceptions

import jobs

class NoSuchJobException(exceptions.Exception):
	"""Exception raised when a job is requested that does not exist."""
	pass

class mysched(sched.scheduler):
	"""Extension of the python schedular to allow locking and all that."""

	def __init__(self, timefunc=time.time, delayfunc=time.sleep):
		"""Allow default time functions and stuff to the scheduler."""
		self.__mutex=threading.RLock()
		sched.scheduler.__init__(self, timefunc, delayfunc)

	def lock(self):
		"""Lock the mutex."""
		if self.__mutex != None:
			self.__mutex.acquire()

	def unlock(self):
		"""Unlock the mutex."""
		if self.__mutex != None:
			self.__mutex.release()

	def __len__(self):
		rv=0
		try:
			self.lock()
			rv=len(self.queue)
		finally:
			self.unlock()
		return rv

	def emptyJobQueue(self):
		"""Empty the job queue."""
		try:
			self.lock()
			self.queue=[]
		finally:
			self.unlock()

	def getQueueList(self):
		"""Get the list of jobs in the queue."""
		rv=[]
		try:
			self.lock()
			rv=self.queue
		finally:
			self.unlock()
		return rv

	def __iter__(self):
		return iter(self.getQueueList())

	def enterabs(self, time, priority, action, argument):
		"""Wrap the enterabs method with a lock."""
		try:
			self.lock()

			sched.scheduler.enterabs(self, time, priority, action, argument)
		finally:
			self.unlock()

	def findJob(self, descriptor):
		"""Find a particular job by descriptor."""
		rv=None
		ql=self.getQueueList()
		for item in ql:
			try:
				j=item[3][0]
				if isinstance(j, jobs.Job):
					if j.getDescriptor() == descriptor:
						rv=j
			except IndexError, e:
				print `e`
		if rv==None:
			raise NoSuchJobException(descriptor)
		return rv

	def run(self):
		"""Run the super queue runner, but with locking."""
		# this is what the super runner does, but it locks when it runs a job

		q=self.queue
		while q:
			time, priority, action, argument = q[0]
			now = self.timefunc()
			if now < time:
				self.delayfunc(time - now)
			else:
				try:
					self.lock()
					del q[0]
					void = apply(action, argument)
				finally:
					self.unlock()
				self.delayfunc(0)
