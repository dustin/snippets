#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: indexGatewayAuth.py,v 1.1 2003/09/05 21:22:03 dustin Exp $
"""

import anydbm
import sys
import string

db=anydbm.open("authdb", "c")

l=sys.stdin.readline()
c=0
while l != '':
	v=string.split(string.strip(l), "\t", 1)
	db[v[0]]=v[1]
	l=sys.stdin.readline()
	c = c + 1
	if c % 1000 == 0:
		print "Processed", c

db.close()
