#!/usr/bin/env python
"""
Walk a sitemap and report timings and stuff.

Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
"""

import sys
import time
import getopt
import random

from twisted.web import client, microdom
from twisted.internet import reactor, protocol, defer, task

R=random.Random()
DEFAULT_SAMPLE_SIZE=5
CONCURRENCY=5
PAGE_REQUEST_COUNT=2

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

def sample_size_for(url):
    return DEFAULT_SAMPLE_SIZE

def get_list(el, name):
    rv=[]
    for outer in el.getElementsByTagName(name):
        rv.append(outer.getElementsByTagName("loc")[0].firstChild().data)
    return rv

def process_sitemap(url):
    start=time.time()
    def f(v):
        print "+ %s ok %.3fs" % (url, time.time() - start)
        doc=microdom.parseXMLString(v)
        pages=get_list(doc, 'url')
        maps=get_list(doc, 'sitemap')
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
        if count < PAGE_REQUEST_COUNT:
            return fetch_page(url, count+1)
    client.downloadPage(url, cf).addCallbacks(
        callback=onSuccess,
        errback=report_error)

def fetch_sitemap(url):
    return client.getPage(url, CountingFile()).addCallbacks(
        callback=process_sitemap(url),
        errback=report_error(url))

if __name__ == '__main__':
    opts, args = getopt.getopt(sys.argv[1:], 'c:m:')
    for o,v in opts:
        if o == '-c':
            semaphore = defer.DeferredSemaphore(tokens=int(v))
        if o == '-m':
            map_semaphore = defer.DeferredSemaphore(tokens=int(v))

    try:
        url = args[0]
    except IndexError:
        sys.stderr.write("Hey.  I need a sitemap URL to start with.\n")
        sys.exit(64)
    fetch_sitemap(url).addBoth(lambda x: reactor.stop())
    reactor.run()
