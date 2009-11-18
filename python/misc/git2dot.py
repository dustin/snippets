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

def store_ref(refs, h, t, name):
    if h in refs:
        if refs[h].index('/') == -1:
            pass
        elif name.index('/') == -1 or name.startswith('origin/'):
            refs[h] = name
    else:
        refs[h] = name

def get_refs():
    args = ['git', 'show-ref', '--abbrev']
    sub=subprocess.Popen(args, stdout=subprocess.PIPE, close_fds=True)

    refs = {}
    for line in sub.stdout:
        h, stuff = line.strip().split()
        try:
            junk, t, name = stuff.split('/', 2)
            store_ref(refs, h, t, name)
        except ValueError:
            pass

    return refs

refs = get_refs()
changes = get_changes(*sys.argv[1:])

print """digraph "g" {"""

for k,v in changes.iteritems():
    for p in v:
        print '\t"%s" -> "%s";' % (refs.get(p, p), refs.get(k, k))

print "}"
