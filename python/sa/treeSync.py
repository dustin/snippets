#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 80ADC2D0-4596-11D9-A6B3-000393CFE6B8

import os
import sys
import stat
import shutil
import getopt
import string
import exceptions

# Global configuration
deepCompare=0
verbose=0
delete=0

# Characters we will allow in the destination tree.
goodChars=string.letters + string.digits + "=,_+.-~ "

def buildGoodSet(goodChars, badChar='_'):
    """Build a translation table that turns all characters not in goodChars
       to badChar"""
    allChars=string.maketrans("", "")
    badchars=string.translate(allChars, allChars, goodChars)
    rv=string.maketrans(badchars, badChar * len(badchars))
    return rv

# Build a translation table that includes only characters
transt=buildGoodSet(goodChars, '_')

def dbgMsg(msg, args):
    global verbose
    if verbose:
        print msg % args

def statSig(f):
    """Compute a state signature tuple."""
    rv=(0,0,0)
    try:
        st=os.stat(f)
        rv=(stat.S_IFMT(st[stat.ST_MODE]),
            st[stat.ST_SIZE],
            st[stat.ST_MTIME])
    except OSError:
        pass

    return rv

def deepCmp(srcf, destf):
    """Compare file contents.

       Return 1 if the contents of the files are the same, 0 if they are
       different."""
    rv=1
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

    return rv

def filesSame(srcf, destf):
    """Check to see if these files are the same.
       This is a variation of something that exists in more modern python.
       Return 1 if the files are the same, 0 if they are different."""
    global deepCompare
    rv = 1

    s1 = statSig(srcf)
    s2 = statSig(destf)

    if s1 == s2:
        # The stat sigs are the same, check if we can do a deep compare
        if deepCompare:
            rv = deepCmp(srcf, destf)
    else:
        # Stat sigs are different, we know it's different
        rv = 0

    return rv

def validateFile(src, dest):
    """Make sure the dest file is the same as the src file (copy if
       necessary)"""
    if filesSame(src, dest) == 0:
        dbgMsg("Copying %s -> %s", (src, dest))
        shutil.copy2(src, dest)

def deltree(path):
    for p in os.listdir(path):
        dfn=os.path.join(path, p)
        if os.path.isdir(dfn):
            deltree(dfn)
        else:
            dbgMsg("Deleting file %s", (path,))
            os.unlink(dfn)
    dbgMsg("Deleting directory %s", (path,))
    os.rmdir(path)

def processDeletes(src, dest, srcnames):
    """Process deletions from the dest directory if any."""
    global delete

    if delete:
        # Index the source names (translated)
        snames={}
        for name in srcnames:
            snames[string.translate(name, transt)]=1

        # Look for missing stuff
        for dname in os.listdir(dest):
            if not snames.has_key(dname):
                dfn=os.path.join(dest, dname)
                if os.path.isdir(dfn):
                    deltree(dfn)
                else:
                    dbgMsg("Deleting file %s", (dfn,))
                    os.unlink(dfn)

def myCopyTree(src, dest):
    """Mirror a tree to another tree."""
    names = os.listdir(src)
    if not os.path.exists(dest):
        dbgMsg("Creating dir %s", (dest,))
        os.mkdir(dest)

    # Deletion processor
    processDeletes(src, dest, names)

    for name in names:
        srcname = os.path.join(src, name)
        # Get the destination name after having gone through the translation
        # table
        destname = os.path.join(dest, string.translate(name, transt))
        if os.path.isdir(srcname):
            myCopyTree(srcname, destname)
        else:
            validateFile(srcname, destname)

#
# UI stuff
#

class UsageError(exceptions.Exception):
    """Exception thrown for invalid usage."""
    pass

def main():
    global verbose
    global delete
    global deepCompare
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'dcv')

        for pair in opts:
            if pair[0] == '-v': verbose=1
            elif pair[0] == '-c': deepCompare=1
            elif pair[0] == '-d': delete=1
    except getopt.GetoptError, e:
        raise UsageError(e)

    try:
        src, dest = args
    except ValueError, e:
        raise UsageError("Need src and dest dirs")

    myCopyTree(src, dest)

def usage():
    print "Usage:  " + sys.argv[0] + " [-d] [-c] [-v] srcdir destdir"
    print " -d enables deletes on the destination directory"
    print " -c enables full file compares (instead of simple stat sigs)"
    print " -v enables verbosity"

#  main thing
if __name__ == '__main__':
    try:
        main()
    except UsageError, e:
        print e
        usage()
