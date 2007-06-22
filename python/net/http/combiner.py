#!/usr/bin/env python
"""
Create an RSS feed comprised of several mercurial RSS changelogs.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import os
import md5
import sys
import time
import urllib
import xml.dom.minidom
from xml.dom.ext import PrettyPrint

import fetch
import feedparser

HGLIST='http://hg.west.spy.net/~dustin/hglist.txt'
HGPATTERN='http://hg.west.spy.net/hg/%s/rss-log'

TMPDIR='/tmp/combiner'

def getList(url=HGLIST, pattern=HGPATTERN):
    f=urllib.urlopen(url)
    try:
        return [pattern % i.strip() for i in f]
    finally:
        f.close()

def __makeFilename(url):
    return os.path.join(TMPDIR, md5.new(url).hexdigest() + ".xml")

def getFeeds(urls):
    rv=[]
    for u in urls:
        if not os.path.exists(TMPDIR):
            os.makedirs(TMPDIR)
        fn=__makeFilename(u)
        fetch.doUpdate(u, fn)
        rv.append(feedparser.parse(fn))
    return rv

def combineFeeds(feeds, maxRv=20):
    rv=[]
    for f in feeds:
        t=f.feed.title.replace(' Changelog', '')
        for e in f.entries:
            d=dict(e)
            d['title']=t + ': ' + e.title
            rv.append(d)
    return sorted(rv, key=lambda x:x['updated_parsed'], reverse=True)[:maxRv]

def formatTimestamp(ts):
    return time.strftime("%a, %d %b %Y %H:%M:%S GMT", time.gmtime(ts))

def generateRss(title, url, descr, items):
    dom = xml.dom.minidom.getDOMImplementation()
    doc=dom.createDocument(None, "rss", None)
    doc.documentElement.setAttribute("version", "2.0")

    channel = doc.createElement("channel")
    doc.documentElement.appendChild(channel)

    def appendText(el, name, val):
        newel = doc.createElement(name)
        newel.appendChild(doc.createTextNode(val))
        el.appendChild(newel)
        return newel

    appendText(channel, "title", title)
    appendText(channel, "link", url)
    appendText(channel, "language", "en-us")
    appendText(channel, "generator", "feedcombiner")
    appendText(channel, "webMaster", "dustin@spy.net")
    appendText(channel, "lastBuildDate", formatTimestamp(time.time()))
    appendText(channel, "ttl", "86400")
    appendText(channel, "description", descr)

    def itemCopy(it, i, key):
        appendText(it, key, i[key].strip())

    for i in items:
        try:
            it = doc.createElement("item")
            itemCopy(it, i, 'link')
            itemCopy(it, i, 'title')
            appendText(it, 'description', i['summary'])
            appendText(it, 'pubDate', i['updated'])
            channel.appendChild(it)
        except KeyError, e:
            print "Error", e, "at", i

    return doc

def __loadFeeds(picklefile='/tmp/feeds_pickled'):
    f=open(picklefile)
    try:
        import pickle
        return pickle.load(f)
    finally:
        f.close()

if __name__ == '__main__':
    feeds=combineFeeds(getFeeds(getList()))
    # feeds=combineFeeds(__loadFeeds())
    doc=generateRss('Project feeds', 'http://hg.west.spy.net/hg/',
        'Recent changes for spy.net projects', feeds)

    tmpfile=sys.argv[1] + '.tmp'
    o=open(tmpfile, 'w')
    try:
        PrettyPrint(doc, stream=o, encoding='UTF-8', indent=' ',
            preserveElements=None)
        os.rename(tmpfile, sys.argv[1])
    except:
        os.unlink(tmpfile)
