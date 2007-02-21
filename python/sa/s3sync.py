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
import traceback
import exceptions

import s3

# How many times to attempt a given upload
NUM_TRIES=5

def sighandler(x, st):
    raise exceptions.Exception("Got signal " + `x`)

def addOne(bucket, relpath):
    signal.alarm(900)
    print "Adding", relpath
    try:
        f=open(relpath)
        ob=s3.S3Object(relpath, f, {})
        bucket.save(ob)
        print "...done"
    finally:
        f.close()
    signal.alarm(0)

def addWithRetry(bucket, relpath):
    tries=0
    success=False

    while not success:
        tries += 1
        try:
            addOne(bucket, relpath)
            success=True
        except:
            if tries >= NUM_TRIES:
                raise
            else:
                traceback.print_exc()
                print "Trying again in %d second(s)" % (tries,)
                time.sleep(tries)

def doAdditions(bucket, remote):
    local=sets.Set()
    added=0

    for d, dirs, files in os.walk("."):
        for f in files:
            relpath=os.path.join(d, f)
            assert relpath[0:2] == './'
            relpath=relpath[2:]
            local.add(relpath)

            if relpath not in remote:
                assert not bucket.has_key(relpath)
                addWithRetry(bucket, relpath)
                added += 1

    return added, local

def __retry(f, args, kwargs):
    for i in range(3):
        try:
            return f(*args, **kwargs)
        except:
            if i == 2:
                raise
            else:
                sys.stderr.write("Failure %d\n" % (i,))
                traceback.print_exc()

def doDeletions(bucket, todelete):
    deleted = 0
    for f in todelete:
        print "Deleting", f
        # bucket.delete(f)
        __retry(bucket.delete, [f], {})
        print "...done"
        deleted += 1
    return deleted

def getAllBucketContents(b):
    rv=sets.Set()
    keys=__retry(bucket.keys, [], {'marker':None, 'delimiter':'^'})
    remote.update(keys)
    while len(keys) == 1000:
        # keys=bucket.keys(marker=keys[-1], delimiter='^')
        keys=__retry(bucket.keys, [], {'marker':keys[-1], 'delimiter':'^'})
        remote.union_update(sets.Set(keys))
    return rv

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
    remote=getAllBucketContents(bucket)
    print "Found %d keys" % (len(remote),)
    os.chdir(top)

    deleted=0

    signal.signal(signal.SIGALRM, sighandler)

    added, local=doAdditions(bucket, remote)
    deleted = doDeletions(bucket, remote.difference(local))

    print "Added:", added
    print "Deleted:", deleted

