#!/usr/bin/env python
"""
Locked DBM access.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: CB57A6E0-3478-11D9-A5FD-000A957659CC

import fcntl
import anydbm

class LockedDBM(object):
    """This class wraps an anydbm and provides locking around the access."""

    def __init__(self, path, flag='r', mode=0666):
        """Initialize an anydbm with a lock."""
        self.path=path
        self.flag=flag
        self.mode=mode
        self.lockfile=open(path + ".lock", "w")

    def __getdb(self):
        return anydbm.open(self.path, self.flag, self.mode)

    def __contains__(self, key):
        return (self.has_key(key))

    def __getitem__(self, key):
        rv=None
        db=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_SH)
            db=self.__getdb()
            rv = db[key]
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
            if db is not None:
                db.close()
        return rv

    def __setitem__(self, key, value):
        rv=None
        db=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_EX)
            db=self.__getdb()
            db[key] = value
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
            if db is not None:
                db.close()

    def keys(self):
        rv=None
        db=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_SH)
            db=self.__getdb()
            rv = db.keys()
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
            if db is not None:
                db.close()
        return rv

    def has_key(self, k):
        rv=None
        db=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_SH)
            db=self.__getdb()
            rv = db.has_key(k)
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
        return rv

    def close(self):
        self.lockfile.close()
