#!/usr/bin/env python
"""

git log -g --pretty=format:'%h %p'

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import fileinput

changes={}

for line in fileinput.input():
    parts=line.strip().split()
    changes[parts[0]] = parts[1:]

for k,v in changes.iteritems():
    for p in v:
        print '%s,%s' % (p, k)
