#!/usr/bin/env python
"""
Utility classes for fetching and processing monitoring statistics from
CMS application servers.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: A38BDEF9-4981-4E1C-B194-47E5D895AC05

import os
import sys
import stat
import time
import urllib

class StatFetcher(object):
    """Fetch stats from the monitor stat servlet."""

    def __init__(self, url):
        """Construct a StatFetcher pointing to the base URL of the stat
        servlet.

        >>> f=StatFetcher("http://w26-1.diag.c26:8080/admin/monitor/stat")
        """
        self.url=url;

    def getValues(self, url=None):
        """Get a dict of all available stats."""
        if url is None:
            url=self.url

        u=urllib.URLopener()
        r=u.open(url)
        h={}
        for l in r.readlines():
            l=l.rstrip()
            (k, v) = l.split(' = ')
            v=int(v)
            h[k] = v
        return h

    def getValue(self, v):
        """Get a particular stat value."""
        h=self.getValues(self.url + "/" + v)
        return h[v]

class ArchivedStat(object):
    """Persist and obtain persistent stats."""

    def __init__(self, host, statName, path="/var/tmp/cms_stat"):
        """Get an archived stat instance for the given host and stat."""
        self.host=host
        self.stat=statName
        self.dir=os.path.join(path, self.host)
        self.path=os.path.join(path, self.host, self.stat)

        try:
            f=None
            try:
                f=open(self.path)
                self.val=int(f.readline().strip())
            finally:
                if f is not None:
                    f.close()
            self.timestamp=os.stat(self.path)[stat.ST_MTIME]
        except:
            self.exception=sys.exc_info()
            # Can't get an archived value.
            self.timestamp=None
            self.val=None

    def getTimestamp(self):
        """Get the timestamp representing the time this value was stored.
        None will be returned if we didn't get a value."""
        return self.timestamp

    def getAge(self):
        """Get the age of the archived data."""
        return time.time() - self.timestamp

    def getValue(self):
        """Get the value that was stored, or None if no value has been
        stored."""
        return self.val

    def store(self, val):
        """Store the given value."""
        if not os.path.exists(self.dir):
            os.makedirs(self.dir)
        f=open(self.path, "w")
        try:
            f.write(str(val) + "\n")
        finally:
            f.close()

    def __repr__(self):
        return "<ArchivedStat " + self.stat + " for host " + self.host \
            + " persists in " + self.path + ">"
