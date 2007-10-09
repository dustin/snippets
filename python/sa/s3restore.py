#!/usr/bin/env python
"""
Restore a backup from my s3 tree.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import os
import sys
import sets
import time
import signal
import httplib
import zipfile
import traceback
import exceptions

import s3

def openBucket(b):
    bucket=s3conn[b]
    assert bucket is not None
    return bucket

def showIndex(bucket):
    a=openBucket(bucket).keys()
    a.sort()
    for e in a:
        print e

def getFile(bucket, name):
    path=os.path.expanduser("~/.s3restore/%s/%s" % (bucket, name))
    if not os.path.exists(path):
        dn=os.path.dirname(path)
        if not os.path.exists(dn):
            os.makedirs(dn)
        print "*** fetching %s/%s" % (bucket, name)
        f=open(path + ".tmp", "w")
        try:
            f.write(openBucket(bucket)[name].data)
            f.close()
            f = None
            os.rename(path + ".tmp", path)
        finally:
            if f is not None:
                f.close()
    return path

def showDay(bucket, day):
    zf=zipfile.ZipFile(getFile(bucket, day))
    for zi in zf.namelist():
        print zi

def restoreFile(bucket, md5bucketname, day, file):
    zf=zipfile.ZipFile(getFile(bucket, day))
    stuff=zf.read(file)
    for line in stuff.split("\n"):
        fn=getFile(md5bucketname, line[0:2] + "/" + line)
        # TODO:  Need decrypt and concatenation here

def doStuff(bucket, md5bucketname, args):
    if len(args) == 0:
        showIndex(bucket)
    elif len(args) == 1:
        showDay(bucket, args[0])
    elif len(args) == 2:
        restoreFile(bucket, md5bucketname, args[0], args[1])
    else:
        raise exceptions.Exception("Don' know what to do with your args")

if __name__ == '__main__':
    # This doesn't exist in my older httplib, but s3 needs it
    try:
        httplib.MAXAMOUNT
    except AttributeError:
        httplib.MAXAMOUNT=8192

    idxbucket, md5bucket, s3id, s3auth = sys.argv[1:5]

    rest=sys.argv[5:]

    s3conn=s3.S3Service(s3id, s3auth)

    doStuff(idxbucket, md5bucket, rest)
