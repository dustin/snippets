#!/usr/bin/env python
"""
Tools for building SAX parsers.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 72CFBE7B-B793-4677-9A81-A6BBD00FB960

import sys
import xml.sax

class ElementHandler(xml.sax.handler.ContentHandler):
    """Interface for element parsing."""

    def __init__(self):
        xml.sax.handler.ContentHandler.__init__(self)
        self.parsers={}

    def getParser(self, name):
        """Get the child parser for the given name"""
        return self.parsers[name]

    def addChild(self, name, child):
        """Add a parsed child object."""

class RootHandler(ElementHandler):

    def __init__(self, type, handler):
        ElementHandler.__init__(self)
        self.parsers[type]=handler
        self.element=None

    def addChild(self, name, child):
        assert self.element is None
        self.element=child

class StackedHandler(xml.sax.handler.ContentHandler):
    """SAX event handler that will delegate results to sub handlers."""

    def __init__(self, rootType, rootHandler):
        """root type and handler expected."""

        xml.sax.handler.ContentHandler.__init__(self)
        self.stack=[]
        self.root=RootHandler(rootType, rootHandler)
        self.stack.append(self.root)

    def startElementNS(self, name, qname, attrs):
        handler=self.stack[-1].getParser(name)
        self.stack.append(handler)
        handler.startElementNS(name, qname, attrs)

    def endElementNS(self, name, qname):
        child=self.stack.pop()
        child.endElementNS(self, name)
        handler=self.stack[-1]
        handler.addChild(name, child)

    def characters(self, content):
        handler=self.stack[-1]
        handler.characters(content)

    def getRootElement(self):
        return self.root.element

class SimpleValueParser(ElementHandler):
    """Parser for simple single values."""

    def __init__(self):
        ElementHandler.__init__(self)
        self.attrs={}
        self.value=""

    def characters(self, content):
        self.value+=content

    def getValue(self):
        return self.value

    def startElementNS(self, name, qname, attrs):
        self.attrs=dict([(k, attrs[k]) for k in attrs.getNames()])
        self.attrs=attrs

    def __getitem__(self, k):
        return self.attrs[k]

    def __repr__(self):
        return "{SimpleValue " + `self.value` + "}"

class SimpleListParser(ElementHandler):
    """Parser for lists of values."""

    def __init__(self, parserFactory=SimpleValueParser):
        """Construct a SimpleListParser with an optional value parser."""
        ElementHandler.__init__(self)
        self.values=[]
        self.factory=parserFactory

    def getParser(self, name):
        return self.factory()

    def getValues(self):
        """Get the contained values."""
        return self.values

    def addChild(self, name, child):
        if isinstance(child, SimpleValueParser):
            self.values.append(child.getValue())
        else:
            self.values.append(child)

class IgnoringParser(ElementHandler):
    """Parser that ignores any of its children."""

    def getParser(self, name):
        print "Ignoring", name
        return IgnoringParser()
