#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: timeServerFinder.py,v 1.1 2003/07/31 04:42:02 dustin Exp $
"""

import sys
import random

class TimeServer:

	def __init__(self, name, exceptions=()):
		self.name=name
		self.exceptions=exceptions

	def __str__(self):
		return "<TimeServer " + self.name + " " + `self.exceptions` + ">"

	__repr__ = __str__

class TimeServerFinder:

	def __init__(self, servers):
		self.nolist={}
		random.shuffle(servers)
		self.servers=servers

	def __aServer(self):
		if len(self.servers) == 0:
			raise "No more servers"
		rv = self.servers[0]
		self.servers=self.servers[1:]
		return rv

	def nextServer(self):
		rv = self.__aServer()
		while self.nolist.has_key(rv.name):
			# print "IGNORING " + `rv`
			rv = self.__aServer()

		# print "Keeping " + `rv`
		for ex in rv.exceptions:
			self.nolist[ex]=1
		return rv

def loadTable(fd):
	rv=[]
	for l in fd.readlines():
		if len(l) < 5 or l[0] == '#':
			continue
		l=l.rstrip()
		parts=l.split()
		t=TimeServer(parts[0], parts[1:])
		rv.append(t)
	return rv

if __name__ == '__main__':
	timeservers=loadTable(sys.stdin)

	tsf=TimeServerFinder(timeservers)
	slist=[]
	while len(slist) < 5:
		slist.append(tsf.nextServer())

	# print "---"
	for s in slist:
		print s.name
