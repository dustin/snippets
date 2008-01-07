#!/usr/bin/env python
"""
Extract articles containing longitude and latitude points from wikipedia.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
from sqlite3 import dbapi2 as sqlite
import xml.sax
import saxkit

ROOT_NS=None
ROOT_EL="feed"

DB=sqlite.connect("wiki.db")
CUR=DB.cursor()

TITLE_PREFIX="Wikipedia: "

ABSTRACT_QUERY="""insert into abstracts (title, abstract) values(?, ?)"""

class OptInHandler(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self, saxkit.IgnoringParser(False))

class DocHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'title')]=saxkit.SimpleValueParser()
        self.parsers[(ROOT_NS, 'abstract')]=saxkit.SimpleValueParser()

    def addChild(self, name, val):
        if name[1] == 'title':
            self.title=val.getValue()
            if self.title.startswith(TITLE_PREFIX):
                self.title = self.title[len(TITLE_PREFIX):]
        elif name[1] == 'abstract':
            self.abstract=val.getValue()

class RootHandler(OptInHandler):
    def __init__(self):
        OptInHandler.__init__(self)
        self.parsers[(ROOT_NS, 'doc')]=DocHandler

    def addChild(self, name, val):
        if isinstance(val, DocHandler):
           try:
               print "%s:  (%s byte abstract)" % (val.title, len(val.abstract))
           except UnicodeEncodeError:
               print "(Found one, but couldn't print it...)"
               pass
           CUR.execute(ABSTRACT_QUERY, (val.title, val.abstract))

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
