#!/usr/bin/env python
"""

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# arch-tag: E34FBB2B-44F4-4E91-AA7F-57CC763139CB
"""

import anydbm
import time
import sys
import google
import md5
import xml.dom.minidom
from xml.dom.ext import PrettyPrint

MAX_AGE=(86400*15)
MAX_RESULTS=20

def getResults(key, query):
    rv=[]

    db=anydbm.open("rssdb", "c")

    try:
        g=google.GoogleSearch(key)
        g.doSearch(query)

        oldest=time.time() - MAX_AGE

        i=0
        for rs in g:
            idx,r = rs
            url=r['URL']
            key=md5.new(url).hexdigest()
            timestamp=time.time()
            if db.has_key(key):
                timestamp=float(db[key])
            else:
                db[key]=str(timestamp)
            if timestamp >= MAX_AGE:
                rv.append((r, idx, timestamp))
                i+=1
            if i > MAX_RESULTS:
                break
    finally:
        db.close()

    return rv

def formatTimestamp(ts):
    return time.strftime("%a, %d %b %Y %H:%M:%S GMT", time.gmtime(ts))

def appendText(doc, el, name, val):
    newel = doc.createElement(name)
    newel.appendChild(doc.createTextNode(val))
    el.appendChild(newel)
    return newel

def appendCData(doc, el, name, val):
    newel = doc.createElement(name)
    newel.appendChild(doc.createCDATASection(val))
    el.appendChild(newel)
    return newel

def appendItem(doc, channel, rsltTuple):
    r, pos, ts = rsltTuple
    it = doc.createElement("item")
    appendText(doc, it, "link", r['URL'])
    appendText(doc, it, "pubDate", formatTimestamp(ts))
    appendText(doc, it, "title", r['title'].strip())
    appendCData(doc, it, "description", r['snippet'].strip())

    channel.appendChild(it)

    return it

def buildDocument(results, keywords):
    dom = xml.dom.minidom.getDOMImplementation()
    doc=dom.createDocument(None, "rss", None)
    doc.documentElement.setAttribute("version", "2.0")

    channel = doc.createElement("channel")
    doc.documentElement.appendChild(channel)

    appendText(doc, channel, "title", "Search results for %s" % (keywords, ))
    appendText(doc, channel, "link", "http://bleu.west.spy.net/~dustin/")
    appendText(doc, channel, "language", "en-us")
    appendText(doc, channel, "generator", "Google2RSS")
    appendText(doc, channel, "webMaster", "dustin@spy.net")
    appendText(doc, channel, "lastBuildDate", formatTimestamp(time.time()))
    appendText(doc, channel, "ttl", "86400")
    appendText(doc, channel, "description",
        "Google search results for %s." % (keywords, ))

    for r in results:
        appendItem(doc, channel, r)

    return doc

if __name__=='__main__':
    results=getResults('2hOO7zk9TTDrPe0fpnxR0Yv/5K66pVHX', sys.argv[1])

    doc=buildDocument(results, sys.argv[1])

    PrettyPrint(doc, stream=sys.stdout, encoding='UTF-8',
        indent=' ', preserveElements=None)
