#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: transReport.py,v 1.1 2003/08/03 20:59:29 dustin Exp $
"""

import exceptions

class MuxProtocol:

	def __init__(self):
		pass

	def getSettings(self, a):
		"""Returns a dict of settings"""
		pass

	def setSettings(self, h):
		"""Takes a dict of settings"""
		pass

	def authenticate(self):
		"""Authenticate the gateway"""
		pass

class FlowException:
	pass

class AutoUpgradeException(FlowException):

	def __repr__(self):
		return "AutoUpgrade in progress"

class Transaction:

	def go(self):
		"""Subclasses override"""
		raise exceptions.NotImplementedError()

	def autoUpgrade(self):
		raise AutoUpgradeException()

class Heartbeat(Transaction):
	pass

class HeartbeatPeriodic(Heartbeat):

	def __init__(self, count=0):
		self.count=count

	def __doIfMod(self, n, op):
		if self.count == 0 or self.count % n == 0:
			op()

	def __doIfNotMod(self, n, op):
		if self.count == 0 or self.count % n != 0:
			op()

	def __reorg(self):
		print "Performing a reorg"

	def __regroup(self):
		print "Performing a regroup"

	def __resetting(self):
		print "Performing a resettingification"

	def go(self):
		print "Performing a HeartbeatPeriodic, number " + `self.count`
		# self.autoUpgrade()

		self.__doIfMod(7, self.__reorg)
		self.__doIfMod(17, self.__regroup)
		self.__doIfMod(31, self.__resetting)

if __name__ == '__main__':
	for i in range(40):
		hb=HeartbeatPeriodic(i)
		try:
			hb.go()
		except FlowException, e:
			print `e`
