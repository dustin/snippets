#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: mergeGatwayList.py,v 1.1 2003/09/05 21:22:05 dustin Exp $
"""

import anydbm
import string
import sys

db=anydbm.open("authdb")

min_ver=[3,1]
https_ver=[3, 4]
new_auth=[3, 1]

# Skip header
l=sys.stdin.readline()
l=sys.stdin.readline()
while l != '':
	a=map(string.strip, l.strip().split(","))
	if a[2] != a[3]:
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
	l=sys.stdin.readline()

db.close()
