#!/usr/bin/env python

import os
import time

class AlreadyLockedException:
	pass

class CannotLockException:
	pass

class PidLock:

	def __init__(self, pidfile):
		self.pidfile=pidfile
		try:
			self.doLock()
		except OSError:
			# If we failed to obtain a lock, verify it's still valid, and
			# if not, reclaim it.
			if self.checkLock():
				raise AlreadyLockedException
			else:
				os.unlink(self.pidfile)
				self.doLock()

	def doLock(self):
		fd=None
		f=None
		self.locked=None
		try:
			fd=os.open(self.pidfile, os.O_WRONLY|os.O_CREAT|os.O_EXCL)
			towrite=str(os.getpid()) + "\n"
			rv=os.write(fd, towrite)
			if rv!=len(towrite):
				self.unlock()
				raise CannotLockException
			self.locked=1
		finally:
			if f!=None:
				f.close()
			if fd!=None:
				os.close(fd)

	def checkLock(self):
		f=file(self.pidfile)
		valid=None
		d=f.readline()
		if d!='':
			pid=int(d)
			try:
				os.kill(pid, 0)
				valid=1
			except OSError, oe:
				if oe[0] != 3:
					print "kill error:  " + str(oe)
					raise AlreadyLockedException
		return valid

	def unlock(self):
		if self.locked:
			os.unlink(self.pidfile)
			self.locked=None

	def __del__(self):
		self.unlock()

if __name__ == '__main__':
	print "Obtaining lock..."
	lock=PidLock("test.pid")
	print "Got it, sleeping..."
	time.sleep(5)
	# print "Unlocking..."
	# lock.unlock()
	print "Done."
