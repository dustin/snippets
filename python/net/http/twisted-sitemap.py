#!/usr/bin/env python
"""
Walk a sitemap and report timings and stuff.

Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
"""

import sys
import time
import getopt
import random

from twisted.web import client, sux
from twisted.internet import reactor, defer, error

R=random.Random()
DEFAULT_SAMPLE_SIZE=5
CONCURRENCY=5
page_request_count=2

map_semaphore = defer.DeferredSemaphore(tokens=1)
semaphore = defer.DeferredSemaphore(tokens=CONCURRENCY)

class CountingFile(object):
    """A file-like object that just counts what's written to it."""
    def __init__(self):
        self.written=0
    def write(self, b):
        self.written += len(b)
    def close(self):
        pass
    def open(self):
        pass
    def read(self):
        return None

class SitemapFile(sux.XMLParser):
    """A file-like thingy that parses sitemap results with SUX."""
    def __init__(self):
        self.maps=[]
        self.pages=[]
        self.connectionMade()
        self.inLoc=False
        self.current=None
        self.data=None
    def write(self, b):
        self.dataReceived(b)
    def close(self):
        self.connectionLost(error.ConnectionDone())
    def open(self):
        pass
    def read(self):
        return None

    # XML Callbacks
    def gotTagStart(self, name, attrs):
        if name == 'loc':
            self.data=[]
            self.inLoc=True
        elif name == 'sitemapindex':
            self.current=self.maps
        elif name == 'urlset':
            self.current=self.pages

    def gotTagEnd(self, name):
        self.inLoc=False
        if name == 'loc':
            self.current.append(''.join(self.data))
            self.data=None

    def gotText(self, data):
        if self.inLoc:
            self.data.append(data)

def sample_size_for(url):
    return DEFAULT_SAMPLE_SIZE

def process_sitemap(url, x):
    start=time.time()
    def f(v):
        print "+ %s ok %.3fs" % (url, time.time() - start)
        pages=x.pages
        maps=x.maps
        print ". found %d pages and %d maps" % (len(pages), len(maps))
        l=[map_semaphore.run(fetch_sitemap, m) for m in maps]
        tofetch=pages
        sample_size = sample_size_for(url)
        if len(tofetch) > sample_size:
            tofetch=R.sample(tofetch, sample_size)
        print ". Fetching %d/%d from %s" % (len(tofetch), len(pages), url)
        for u in tofetch:
            l.append(semaphore.run(fetch_page, u))
        rv = defer.DeferredList(l)
        return rv
    return f

def report_error(url):
    start=time.time()
    def f(v):
        print "! Error on %s: %s" % (url, v.getErrorMessage())
        sys.stdout.flush()
    return f

def fetch_page(url, count=1):
    cf = CountingFile()
    start = time.time()
    def onSuccess(value):
        print "- %d %s ok %d bytes in %.3f" % (count,
            url, cf.written, time.time() - start)
        if count < page_request_count:
            return fetch_page(url, count+1)
    return client.downloadPage(url, cf).addCallbacks(
        callback=onSuccess,
        errback=report_error)

def fetch_sitemap(url):
    x=SitemapFile()
    return client.downloadPage(url, x).addCallbacks(
        callback=process_sitemap(url, x),
        errback=report_error(url))

if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], 'c:m:n:')
    for o,v in opts:
        if o == '-c':
            semaphore = defer.DeferredSemaphore(tokens=int(v))
        if o == '-m':
            map_semaphore = defer.DeferredSemaphore(tokens=int(v))
        if o == '-n':
            page_request_count = int(v)

    try:
        url = args[0]
    except IndexError:
        sys.stderr.write("Hey.  I need a sitemap URL to start with.\n")
        sys.exit(64)
    fetch_sitemap(url).addBoth(lambda x: reactor.stop())
    reactor.run()
