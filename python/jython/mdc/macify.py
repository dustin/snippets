#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: macify.py,v 1.1 2003/09/26 01:13:24 dustin Exp $
"""

import anydbm
import sys

db=anydbm.open("macdb")

l=sys.stdin.readline()
while l != '':
	if l[0] == '\t':
		m=l[1:3] + l[4:6] + l[7:9]
		vendor=''
		if db.has_key(m):
			vendor=db[m]
		print "\t" + l.strip() + "\t" + vendor
	else:
		print l.strip()
	l=sys.stdin.readline()

db.close()
