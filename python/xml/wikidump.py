#!/usr/bin/env python
"""
Extract articles containing longitude and latitude points from wikipedia.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import re
import sys
import xml.sax
import saxkit

ROOT_NS="http://www.mediawiki.org/xml/export-0.3/"
ROOT_EL="mediawiki"

REGEX=re.compile(r"^\s+(Latitude|Longitude)\s+=\s+(.*)\s+\|\s*$")

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
            for l in val.getValue().split("\n"):
                v=REGEX.match(l)
                if v:
                    g=v.groups()
                    setattr(self.sup, g[0].lower(), g[1])

class PageHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'title')]=saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'revision')]=RevisionHandler(self)

        self.latitude=None
        self.longitude=None

    def addChild(self, name, val):
        if name[1] == 'title':
            self.title=val.getValue()

    def __unicode__(self):
        return "<Page name='%s', latitude=%s, longitude=%s>" % (self.title,
            self.latitude, self.longitude)

class RootHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'page')]=PageHandler

    def addChild(self, name, val):
        if isinstance(val, PageHandler) and val.latitude:
            print unicode(val)

if __name__ == '__main__':
    handler=saxkit.StackedHandler(
        (ROOT_NS, ROOT_EL), RootHandler())
    parser=xml.sax.make_parser()
    parser.setFeature(xml.sax.handler.feature_namespaces, True)
    parser.setContentHandler(handler)
    parser.parse(sys.stdin)
    sm=handler.getRootElement()
