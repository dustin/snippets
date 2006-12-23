#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: D6F9771C-802B-448C-A9DC-5CC50B4B830E

import os
import sys
import sets

files=sets.Set([x.strip()[3:] for x in sys.stdin])

toremove=0
found=0
for dirpath, dirnames, filenames in os.walk("md5s"):
    s=sets.Set(filenames)
    found += len(s)
    d=s.difference(files)
    toremove += len(d)

    for f in d:
        os.unlink(os.path.join(dirpath, f))

print "Found %d, removed %d" % (found, toremove)
