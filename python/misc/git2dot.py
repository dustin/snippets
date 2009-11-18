#!/usr/bin/env python
"""
Make a dot diagram of a git repo.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import subprocess

def get_changes(*args):
    args = ['git', 'log', "--pretty=format:%h %p"] + list(args)
    sub=subprocess.Popen(args, stdout=subprocess.PIPE, close_fds=True)

    changes={}

    for line in sub.stdout:
        parts=line.strip().split()
        changes[parts[0]] = parts[1:]

    return changes

print """digraph "g" {"""

for k,v in get_changes().iteritems():
    for p in v:
        print '\t"%s" -> "%s";' % (p, k)

print "}"
