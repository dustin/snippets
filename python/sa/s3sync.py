#!/usr/bin/env python
"""
Sync a tree with s3.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 630-542-11-9395-0030187026

import os
import sys
import sets
import time
import signal
import httplib
import exceptions

import s3

def sighandler(x, st):
    raise exceptions.Exception("Got signal " + `x`)

if __name__ == '__main__':
    # This doesn't exist in my older httplib, but s3 needs it
    try:
        httplib.MAXAMOUNT
    except AttributeError:
        httplib.MAXAMOUNT=8192

    top, bucket, s3id, s3auth = sys.argv[1:]

    c=s3.S3Service(s3id, s3auth)
    bucket=c[bucket]
    assert bucket is not None
    remote=sets.Set(bucket.keys())
    local=sets.Set()

    added=0
    deleted=0

    signal.signal(signal.SIGALRM, sighandler)

    try:
        os.chdir(top)
        for d, dirs, files in os.walk("."):
            for f in files:
                relpath=os.path.join(d, f)
                assert relpath[0:2] == './'
                relpath=relpath[2:]
                local.add(relpath)

                if relpath not in remote:
                    signal.alarm(15)
                    print "Adding", relpath
                    f=open(relpath)
                    ob=s3.S3Object(relpath, f, {})
                    bucket.save(ob)
                    f.close()
                    print "...done"
                    signal.alarm(0)
                    added += 1

        for f in remote.difference(local):
            print "Deleting", f
            bucket.delete(f)
            print "...done"
            deleted += 1
    finally:
        print "Added:", added
        print "Deleted:", deleted

