#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import sys
import string

themap={}
mapf=open(sys.argv[1])
for l in mapf.readlines():
    l=string.strip(l)
    pn,sn=string.split(l, ",")
    themap[sn]=pn
mapf.close()

l=sys.stdin.readline()
while l != '':
    k=None
    sn=l[0:12]
    rest=l[13:]
    try:
        k=themap[sn]
    except KeyError:
        k="UNKNOWN"
    sys.stdout.write(k + " " + sn + " err " + rest)
    l=sys.stdin.readline()
