#!/usr/bin/env python

from __future__ import generators

import anydbm
import time

def walkDbm(dbm):
	finished=None
	k,v=dbm.first()
	while 1:
		yield k,v
		try:
			k,v=dbm.next()
		except KeyError:
			raise StopIteration

olddb=anydbm.open("newsdb")
newdb=anydbm.open("newsdb.new", "c")

n=time.time()
for k,v in walkDbm(olddb):
	try:
		if k[0]=='a':
			age=n-float(v)
			# print "Age of " + k + ":  " + str(age)
			# Only remember stuff less than 14 days
			if age < (14*86400):
				newdb[k]=v
			else:
				print "Age of " + k + ":  " + str(age) + ", removing."
		else:
			# Not an article
			newdb[k]=v
	except ValueError, e:
		print "ValueError:  " + str(e)
