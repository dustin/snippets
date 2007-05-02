#!/usr/bin/env python
"""
Cleanup duplicate files as found in an md5 list.

The md5s are found using the following ocaml programs:
* digestfiles
* multimatch

Recipe:

digestfiles /some/path/ > digests.txt
awk '{print $1}' digests.txt | sort | uniq -d > digests.dups
multimatch digests.dups digests.txt > (sys.arg[1] to this script)

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import sys
import os
import time

def manualProcess(v):
	print "------"
	for i in range(len(v)):
		print "\t", i, v[i]
	sys.stdout.write("Which do you want to keep?  ")
	sys.stdout.flush()
	x=int(sys.stdin.readline().strip())
	print "Keeping", x
	for i in range(len(v)):
		print "\t", i, v[i]
		if i != x:
			print "**** Deleting", v[i]
			os.unlink(v[i])

def process(v):
	rv=None
	# try to do this automatically
	tmpv=filter(lambda x: x.find("./rip") != 0, v)
	if(len(tmpv) == 1):
		print "Can autoclean", v, "to", tmpv
		for fn in v:
			if(fn.find("./rip") == 0):
				print "Removing", fn
				time.sleep(0.75)
				os.unlink(fn)
	else:
		# manualProcess(v)
		rv=v
	return rv

# Load them
files={}
f=open(sys.argv[1])
for l in f.readlines():
	l=l.strip()
	m,fn=l.split("\t")
	if(files.has_key(m)):
		files[m].append(fn)
	else:
		files[m]=[fn]

f.close()

thelist=[]
for k,v in files.iteritems():
	vals=filter(os.path.exists, v)
	vals.sort()
	if(len(vals) > 1):
		thelist.append(vals)
	else:
		# print k, "is cleaned up with only", vals
		pass

thelist.sort()
newlist=[]

print len(thelist), "songs to clean up."
# Loop and cleanup
for v in thelist:
	rv=process(v)
	if rv is not None:
		newlist.append(rv)

for v in newlist:
	manualProcess(v)
