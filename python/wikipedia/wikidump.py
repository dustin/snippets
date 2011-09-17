#!/usr/bin/env python
"""
Extract articles containing longitude and latitude points from wikipedia.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
from sqlite3 import dbapi2 as sqlite
import xml.sax
import saxkit

import couchdb

import coordsearch

ROOT_NS="http://www.mediawiki.org/xml/export-0.4/"
ROOT_EL="mediawiki"

URL = 'http://minimata:5984/'
COUCH = couchdb.Server(URL, full_commit=False)
DB = COUCH['wikipedia']

WAITING_FOR = None

class OptInHandler(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self, saxkit.IgnoringParser(False))

class ContributorHandler(OptInHandler):

    def __init__(self, sup):
        OptInHandler.__init__(self)
        self.sup = sup
        self.parsers[(ROOT_NS, 'username')] = saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'id')] = saxkit.SimpleValueParser()

    def addChild(self, name, val):
        if name[1] == 'username':
            self.sup.revdata['contributor'] = val.getValue()
        elif name[1] == 'id':
            self.sup.revdata['contributorid'] = int(val.getValue())

class RevisionHandler(OptInHandler):
    def __init__(self, sup):
        OptInHandler.__init__(self)
        self.sup=sup
        self.parsers[(ROOT_NS, 'text')]=saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'comment')]=saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'timestamp')]=saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'id')]=saxkit.SimpleValueParser()

    def checkCoords(self, s):
        try:
            v = coordsearch.GEO_RE.search(s)
            if v:
                groups = v.groups()
                pv = coordsearch.parse(groups[1])
                if pv:
                    self.sup.geo = { "type": "Feature",
                                     "geometry": {"type": "Point",
                                                  "coordinates": [pv[1], pv[0]]}}
        except IndexError:
            pass
        except ValueError:
            pass

    def addChild(self, name, val):
        if isinstance(val, saxkit.SimpleValueParser):
            s=val.getValue()
            if name[1] == 'text':
                self.sup.text=s
                self.checkCoords(s)
            elif name[1] == 'comment':
                self.sup.revdata['comment'] = s
            elif name[1] == 'timestamp':
                self.sup.revdata['ts'] = s
            elif name[1] == 'id':
                self.sup.revdata['id'] = int(s)

    def getParser(self, name):
        rv=None
        if name == (ROOT_NS, 'contributor'):
            rv=ContributorHandler(self.sup)
        else:
            rv=OptInHandler.getParser(self, name)
        return rv

class PageHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'title')]=saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'id')] = saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'is_redirect')]=saxkit.IgnoringParser()

        self.was_seen=False
        self.is_redirect = False
        self.geo = None
        self.revdata = {}
        self.interesting = True

    def addChild(self, name, val):
        if name[1] == 'title':
            self.title=val.getValue()
        elif name[1] == 'redirect':
            self.is_redirect = True
        elif name[1] == 'id':
            self.wpId = int(val.getValue())

    def has_seen(self, title):
        assert title
        global WAITING_FOR
        self.was_seen = bool(WAITING_FOR)
        if self.was_seen:
            self.interesting = False
        if WAITING_FOR == title:
            print "Enabling, found", title
            WAITING_FOR = None
        return self.was_seen

    def getParser(self, name):
        rv=None
        if name == (ROOT_NS, 'revision') and not self.has_seen(self.title):
            rv=RevisionHandler(self)
        else:
            rv=OptInHandler.getParser(self, name)
        return rv

    def __unicode__(self):
        l = 0
        if hasattr(self, 'text'):
            l = len(self.text)
        return "<Page name='%s', %d bytes>" % (self.title, l)

class RootHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'page')]=PageHandler
        self.allDocs = []
        self.total = 0

    def __del__(self):
        self.commit()

    def commit(self):
        self.total += len(self.allDocs)
        print "Committing after", self.total, "docs"
        if self.allDocs:
            DB.update(self.allDocs)
            self.allDocs = []

    def addChild(self, name, val):
        if isinstance(val, PageHandler):
            if val.interesting:
                try:
                    print unicode(val)
                except UnicodeEncodeError:
                    print "(Found one, but couldn't print it...)"
                    pass
                doc = couchdb.Document(_id=val.title,
                                       text=val.text,
                                       revinfo=val.revdata)
                if val.is_redirect:
                    doc['is_redirect'] = True
                if val.geo:
                    doc['geo'] = val.geo
                self.allDocs.append(doc)
                if len(self.allDocs) >= 10000:
                    self.commit()


if __name__ == '__main__':

    try:
        WAITING_FOR = DB.changes(since=(DB.info()['update_seq'] - 1))['results'][0]['id']
        print "Waiting for", WAITING_FOR
    except:
        pass

    # Load seen titles
    handler=saxkit.StackedHandler(
        (ROOT_NS, ROOT_EL), RootHandler())
    parser=xml.sax.make_parser()
    parser.setFeature(xml.sax.handler.feature_namespaces, True)
    parser.setContentHandler(handler)
    parser.parse(sys.stdin)
    sm=handler.getRootElement()
