#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: mergeGatwayList.py,v 1.2 2003/09/11 00:54:12 dustin Exp $
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
		wanted[string.strip(l)]=1
		l=f.readline()
	f.close()
	sys.stderr.write("Only want " + `len(wanted)` + " records\n")

# Skip header
# l=sys.stdin.readline()
# l=sys.stdin.readline()
l=sys.stdin.readline()
while l != '':
	a=map(string.strip, string.split(string.strip(l), ","))
	batch=a[0]
	org=a[1]
	sn=a[2]
	ver=a[3]
	# hpip=a[4]
	# sip=a[5]
	if len(a) < 3:
		sys.stderr.write("This line looks dumb:  " + `a` + "\n")
	elif wanted is not None and not wanted.has_key(sn):
		# sys.stderr.write("Don't want " + sn + "\n")
		pass
	else:
		ev=map(int, string.split(ver, "."))
		if ev >= min_ver:
			if(db.has_key(sn)):
				authinfo=string.split(db[sn], "\t")
				prot="http://"
				if ev >= https_ver:
					prot="https://"
				url = prot + sn + ".mdc.cms.2wire.com" + ":50001/"
				authIndex=0
				if ev >= new_auth:
					authIndex=1
				print org, sn, ver, authinfo[authIndex], url
			else:
				sys.stderr.write("No auth for " + sn + "\n")
		else:
			sys.stderr.write(sn + " does not meet version requirements: "
				+ ver + "\n")
	l=sys.stdin.readline()

db.close()
