#!/usr/bin/env python

import time

class Stats:
	def __init__(self, total):
		self.done=0
		self.left=total
		self.startTime=time.time()
		self.totaltime=0
		self.lastproctime=0
		self.lasttime=0
		self.statname=None

	def click(self):
		self.left=self.left-1

	def start(self):
		self.lasttime = time.time()

	def stop(self):
		self.lastproctime = time.time()-self.lasttime
		self.done= self.done +1
		self.totaltime = self.totaltime + self.lastproctime

	def getLastTime(self):
		return str(self.lastproctime) + "s"

	def setStatName(self, to):
		self.statname=to

	def getStats(self):
		avgproctime= self.totaltime / self.done
		estimate = avgproctime * self.left

		print "Total time is " + str(self.totaltime)

		if self.statname == None:
			rv=""
		else:
			rv=self.statname + " "

		rv+="Avg=%.2fs, Remaining: %d, est %.2fs (%s)" % \
			(avgproctime, self.left, estimate, \
				time.ctime( time.time() + estimate ))
		return(rv)

def main():
	stats=Stats(10)
	stats.setStatName("Sleep")
	for i in range(10):
		stats.start()
		time.sleep(1)
		stats.stop()
		stats.click()
		print stats.getStats()

if __name__ == "__main__":
	main()
