#!/usr/bin/env python
"""
Remove RCS keywords from files.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
arch-tag: D7C803A6-1957-11D9-8437-000393CFE6B8
"""

import sys
import os

keywords=['$Id', '$Revision', '$Author', '$DateTime']

def mkdirs(path):
    if not os.path.exists(path):
        head, tail = os.path.split(path)
        mkdirs(head)
        os.mkdir(path)

def containsKeyword(l):
    rv=False
    for k in keywords:
        if l.find(k) >= 0:
            rv=True
    return rv

def process(filename):
    f=open(filename)
    lines=f.readlines()
    f.close()
    linesout=[]
    for l in lines:
        if not containsKeyword(l):
            linesout.append(l)
    # Make sure we have a place for this
    # outfile=base + "/" + filename
    # mkdirs(os.path.dirname(outfile))
    outfile=filename
    print "Writing", outfile
    f=open(outfile, "w")
    f.writelines(linesout)
    f.close()

if __name__ == '__main__':
    for filename in sys.argv[1:]:
        process(filename)
