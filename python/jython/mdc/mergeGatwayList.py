#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: mergeGatwayList.py,v 1.1 2003/09/26 01:13:25 dustin Exp $
"""

import anydbm
import string
import sys

db=anydbm.open("authdb")

min_ver=[3,1]
https_ver=[3, 4]
new_auth=[3, 1]

wanted=None
if len(sys.argv) > 1:
	wanted={}
	f=file(sys.argv[1])
	l=f.readline()
	while l != '':
		wanted[l.strip()]=1
		l=f.readline()
	f.close()
	sys.stderr.write("Only want " + `len(wanted)` + " records\n")

# Skip header
l=sys.stdin.readline()
l=sys.stdin.readline()
while l != '':
	a=map(string.strip, l.strip().split(","))
	if wanted is not None and not wanted.has_key(a[0]):
		# sys.stderr.write("Don't want " + a[0] + "\n")
		pass
	elif a[2] != a[3]:
		sys.stderr.write(a[2] + " != " + a[3] + "\n")
	else:
		ev=map(int, a[1].split("."))
		if ev >= min_ver:
			if(db.has_key(a[0])):
				authinfo=db[a[0]].split("\t")
				prot="http://"
				if ev >= https_ver:
					prot="https://"
				url = prot + a[2] + ":50001/"
				authIndex=0
				if ev >= new_auth:
					authIndex=1
				print a[0], a[1], authinfo[authIndex], url
			else:
				sys.stderr.write("No auth for " + a[0] + "\n")
		else:
			sys.stderr.write(a[0] + " does not meet version requirements: "
				+ a[1] + "\n")
	l=sys.stdin.readline()

db.close()
