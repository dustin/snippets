#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import os
import sys
import stat

import zipfile

def initMd5s(md5dir):
    h={}
    for dirpath, dirnames, filenames in os.walk(md5dir):
        for f in filenames:
            s=os.stat(os.path.join(dirpath, f))
            h[s[stat.ST_INO]]=f
    return h

def doTable(d, table, h, zf):
    files=os.listdir(".")
    files.sort()
    seq=[]
    for f in files:
        inode=os.stat(f)[stat.ST_INO]
        seq.append(h[inode])

    zf.writestr(d + '/' + table, '\n'.join(seq))

def doDate(d, h, zf):
    for table in os.listdir("."):
        os.chdir(table)
        doTable(d, table, h, zf)
        os.chdir("..")

def walkTrees(top, h, outdir):
    os.chdir(top)
    dates=os.listdir(".")
    dates.sort()
    rv={}
    for d in dates:
        zfn=os.path.join(outdir, d) + ".zip"
        if not os.path.exists(zfn):
            zf=zipfile.ZipFile(zfn, mode="w", compression=zipfile.ZIP_DEFLATED)
            os.chdir(d)
            doDate(d, h, zf)
            os.chdir("..")
            zf.close()
    return rv

if __name__ == '__main__':
    h=initMd5s(sys.argv[1])
    stuff=walkTrees(sys.argv[2], h, sys.argv[3])
