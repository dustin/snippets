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
        self.db=anydbm.open(path, flag, mode)
        self.lockfile=open(path + ".lock", "w")

    def __contains__(self, key):
        return (self.has_key(key))

    def __getitem__(self, key):
        rv=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_SH)
            rv = self.db[key]
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
        return rv

    def __setitem__(self, key, value):
        rv=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_EX)
            self.db[key] = value
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)

    def keys(self):
        rv=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_SH)
            rv = self.db.keys()
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
        return rv

    def has_key(self, k):
        rv=None
        try:
            fcntl.flock(self.lockfile, fcntl.LOCK_SH)
            rv = self.db.has_key(k)
        finally:
            fcntl.flock(self.lockfile, fcntl.LOCK_UN)
        return rv

    def close(self):
        self.db.close()
        self.lockfile.close()

    # Deal with anything else
    def __getattr__(self, name):
        return getattr(self.db, name)
