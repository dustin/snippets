#!/usr/bin/env python

import threading
import time

class TestThing:
	def __init__(self):
		self.v=0

	def increment(self):
		if self.v == 1:
			raise "EVERYTHING IS BROKEN!"
		self.v+=1

	def decrement(self):
		self.v-=1

class TestThread(threading.Thread):

	mutex=None

	def __init__(self, id, tt):
		threading.Thread.__init__(self)
		# Instantiate once
		if TestThread.mutex == None:
			TestThread.mutex = threading.Lock()

		self._threadId=id
		self.tt=tt
		self.setName("TestThread #" + str(self._threadId))
		self.setDaemon(1)

	def getLockedResource(self):
		# print self.getName() + " waiting for lock."

		TestThread.mutex.acquire()
		# print self.getName() + " obtained lock."
		self.tt.increment()
		# time.sleep(.25)
		# print self.getName() + " unlocking."
		self.tt.decrement()
		TestThread.mutex.release()

	def run(self):
		i=0
		while 1:
			self.getLockedResource()
			i+=1
			# print "Thread running:  " + self.getName() + " i=" + str(i)

if __name__ == '__main__':
	tt=TestThing()
	for i in range(0,10):
		t=TestThread(i, tt)
		t.start()
	time.sleep(60)
