#!/usr/bin/env python

import anydbm
import time

olddb=anydbm.open("newsdb")
newdb=anydbm.open("newsdb.new", "c")

n=time.time()
for k in olddb.keys():
	try:
		if k[0]=='a':
			age=n-float(olddb[k])
			# print "Age of " + k + ":  " + str(age)
			# Only remember stuff less than 14 days
			if age < (14*86400):
				newdb[k]=olddb[k]
			else:
				print "Age of " + k + ":  " + str(age) + ", removing."
		else:
			# Not an article
			newdb[k]=olddb[k]
	except KeyError, ke:
		print "Key error on " + k + "(" + str(ke) + ")"
	except ValueError, ve:
		print "Value error on " + k + "(" + str(ve) + ")"
