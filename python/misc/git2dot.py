#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import fileinput

changes={}

for line in fileinput.input():
    parts=line.strip().split()
    changes[parts[0]] = parts[1:]

print """digraph "g" {"""

for k,v in changes.iteritems():
    for p in v:
        print '\t"%s" -> "%s";' % (p, k)

print "}"
