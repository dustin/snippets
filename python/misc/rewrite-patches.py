#!/usr/bin/env python
"""
Order a set of git format-patch patches in chronological order.

Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
"""

import os
import sys
import time

FMT='%a, %d %b %Y %H:%M:%S'

def schwartz(fn):
    ts=None
    t=None
    try:
        f=open(fn)
        l=f.readline().strip()
        while l and not l.startswith('Date: '):
            l=f.readline().strip()
        f.close()
        t=l[5:-6].strip()
        ts=time.strptime(t, FMT)
    except:
        print "--- Failed to parse %s (%s)" % (fn, t)
        pass
    return (ts, fn)

files = os.listdir(".")

schwartzed=[schwartz(x) for x in files]
filtered = [x for x in schwartzed if x[0]]
sorted = [x[1] for x in sorted(filtered)]

n=0
for f in sorted:
    base=f[5:]
    dest="../sorted-patches/%04d-%s" % (n, base)
    print "%s -> %s" % (f, dest)
    os.link(f, dest)
    n += 1
