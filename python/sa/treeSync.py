#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 80ADC2D0-4596-11D9-A6B3-000393CFE6B8

import os
import sys
import stat
import shutil

# Compute a stat signature tuple
def statSig(f):
    rv=(0,0,0)
    try:
        st=os.stat(f)
        rv=(stat.S_IFMT(st[stat.ST_MODE]),
            st[stat.ST_SIZE],
            st[stat.ST_MTIME])
    except OSError:
        pass

    return rv

# This is a variation of something that exists in more modern python
def filecmp(srcf, destf):
    rv = 1

    s1 = statSig(srcf)
    s2 = statSig(destf)

    # If the stat sigs are the same, reverting to file compares
    if s1 == s2:

        # 8k buffers
        bufsize=8*1024
        f1=open(srcf, "rb")
        f2=open(destf, "rb")
        keepGoing=1
        while keepGoing:
            b1 = f1.read(bufsize)
            b2 = f2.read(bufsize)

            rv = (b1 == b2)

            # If we found a difference, or end of file, stop
            if rv == 0 or len(b1) == 0:
                keepGoing = 0
    else:
        # Stat sigs are different, we know it's different
        rv = 0

    return rv

# Individual file validation
def validateFile(src, dest):
    if filecmp(src, dest) == 0:
        print "Copying " + src + " -> " + dest
        shutil.copy2(src, dest)

# Copy the src tree to the dest tree.
def myCopyTree(src, dest):
    names = os.listdir(src)
    if not os.path.exists(dest):
        print "Creating dir " + dest
        os.mkdir(dest)
    for name in names:
        srcname = os.path.join(src, name)
        destname = os.path.join(dest, name)
        if os.path.isdir(srcname):
            myCopyTree(srcname, destname)
        else:
            validateFile(srcname, destname)

#  main thing
if __name__ == '__main__':
    src=sys.argv[1]
    dest=sys.argv[2]

    myCopyTree(src, dest)
