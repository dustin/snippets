#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 2579895-5427-11-9395-0030187026

import os
import sys
import stat

def initMd5s(md5dir):
    h={}
    for dirpath, dirnames, filenames in os.walk(md5dir):
        for f in filenames:
            s=os.stat(os.path.join(dirpath, f))
            h[s[stat.ST_INO]]=f
    return h

def doTable(d, table, h, datedir):
    files=os.listdir(".")
    files.sort()
    seq=[]
    for f in files:
        inode=os.stat(f)[stat.ST_INO]
        seq.append(h[inode])
    f=open(os.path.join(datedir, table), "w")
    f.write('\n'.join(seq))
    f.close()

def doDate(d, h, datedir):
    for table in os.listdir("."):
        os.chdir(table)
        doTable(d, table, h, datedir)
        os.chdir("..")

def walkTrees(top, h, outdir):
    os.chdir(top)
    dates=os.listdir(".")
    dates.sort()
    rv={}
    for d in dates:
        datedir=os.path.join(outdir, d)
        if not os.path.exists(datedir):
            os.mkdir(datedir)
        os.chdir(d)
        doDate(d, h, datedir)
        os.chdir("..")
    return rv

if __name__ == '__main__':
    h=initMd5s(sys.argv[1])
    stuff=walkTrees(sys.argv[2], h, sys.argv[3])
