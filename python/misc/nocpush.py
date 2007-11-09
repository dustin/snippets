#!/usr/bin/env python
"""
Push code from an mq to perforce.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import os
import atexit
import mercurial.patch

HGDIR=os.environ['NOC_HGDIR']
P4DIR=os.environ['NOC_P4DIR']
TMPFILENAME="/tmp/patch." + str(os.getpid())

# The Mac OS X patch will actually try to talk to perforce.  We don't want that
os.environ['PATCH_GET']='0'

def runCmd(cmd):
    print "Running", cmd
    x=os.system(cmd)
    if os.WEXITSTATUS(x) != 0:
        raise SystemError, ("Bad exit code from ``%s'':  %d" %
            (cmd, os.WEXITSTATUS(x)))

os.chdir(HGDIR)
runCmd("hg export `hg qtop` > %s" % TMPFILENAME)
atexit.register(os.unlink, TMPFILENAME)

# Let us grok the patch
f=open(TMPFILENAME)
num, patches=mercurial.patch.readgitpatch(f)
f.close()

# Classify the above by type
patchhash={}
for p in patches:
    a=patchhash.get(p.op, [])
    a.append(p)
    patchhash[p.op] = a

# Now, over in perforce
os.chdir(P4DIR)
runCmd("p4 sync")
runCmd("p4 resolve -am")

# Set up our changes
toedit=patchhash.get('MODIFY',[])
if toedit:
    cmd=' '.join(['p4 edit'] + ["'%s'" % p.path for p in toedit])
    runCmd(cmd)

for p in patchhash.get('RENAME', []):
    runCmd("p4 integrate '%s' '%s'" % (p.oldpath, p.path))
    if p.lineno:
        runCmd("p4 edit '%s'" % p.path)

# Apply the patch
runCmd("patch -p1 < %s" % TMPFILENAME)

toadd=patchhash.get('ADD',[])
if toadd:
    cmd=' '.join(['p4 add'] + ["'%s'" % p.path for p in toadd])
    runCmd(cmd)

todelete=["'%s'" % p.path for p in patchhash.get('DELETE', [])] + \
    ["'%s'" % p.oldpath for p in patchhash.get('RENAME', [])]

if todelete:
    cmd=' '.join(['p4 delete'] + todelete)
    runCmd(cmd)

