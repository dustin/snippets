#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: indexGatewayAuth.py,v 1.1 2003/09/26 01:13:23 dustin Exp $
"""

import anydbm
import sys

db=anydbm.open("authdb", "c")

l=sys.stdin.readline()
while l != '':
	v=l.strip().split("\t", 1)
	db[v[0]]=v[1]
	l=sys.stdin.readline()

db.close()
