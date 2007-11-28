#!/usr/bin/env python
"""
Extract articles containing longitude and latitude points from wikipedia.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
from sqlite3 import dbapi2 as sqlite
import xml.sax
import saxkit

ROOT_NS="http://www.mediawiki.org/xml/export-0.3/"
ROOT_EL="mediawiki"

DB=sqlite.connect("wiki.db")
CUR=DB.cursor()

HAS_SEEN_QUERY="""
    select 1 from seen_articles where title = ?
"""
SEEN_QUERY="""
    insert into seen_articles values(?)
"""

INTERESTING_QUERY="""
    insert into interesting_pages(title, article_text) values(?, ?)
"""

class OptInHandler(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self, saxkit.IgnoringParser(False))

class RevisionHandler(OptInHandler):
    def __init__(self, sup):
        OptInHandler.__init__(self)
        self.sup=sup
        self.parsers[(ROOT_NS, 'text')]=saxkit.SimpleValueParser()

    def addChild(self, name, val):
        if isinstance(val, saxkit.SimpleValueParser):
            s=val.getValue()
            self.sup.text=s
            if s.find("{{coord|") > 0 or s.find("{{Geolinks") > 0:
                self.sup.interesting=True

class PageHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'title')]=saxkit.SimpleValueParser()

        self.was_seen=False
        self.interesting=False

    def addChild(self, name, val):
        if name[1] == 'title':
            self.title=val.getValue()

    def has_seen(self, title):
        assert title
        CUR.execute(HAS_SEEN_QUERY, (title, ))
        if CUR.fetchall():
            self.was_seen=True
        return self.was_seen

    def getParser(self, name):
        rv=None
        if name == (ROOT_NS, 'revision') and not self.has_seen(self.title):
            rv=RevisionHandler(self)
        else:
            rv=OptInHandler.getParser(self, name)
        return rv

    def __unicode__(self):
        return "<Page name='%s', %d bytes>" % (self.title, len(self.text))

class RootHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'page')]=PageHandler

    def addChild(self, name, val):
        if isinstance(val, PageHandler):
            if not val.was_seen:
                CUR.execute(SEEN_QUERY, (val.title,))
            if val.interesting:
                print unicode(val)
                CUR.execute(INTERESTING_QUERY, (val.title, val.text))
                DB.commit()

if __name__ == '__main__':
    handler=saxkit.StackedHandler(
        (ROOT_NS, ROOT_EL), RootHandler())
    parser=xml.sax.make_parser()
    parser.setFeature(xml.sax.handler.feature_namespaces, True)
    parser.setContentHandler(handler)
    parser.parse(sys.stdin)
    sm=handler.getRootElement()

    DB.commit()
    CUR.close()
    DB.close()
