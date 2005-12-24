#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 187941-7374-11-510-0030187026

import sys
import saxkit
import xml.sax

class Comps(saxkit.ElementHandler):

    groups={}

    def getParser(self, name):
        rv=None
        if name == (None, u'grouphierarchy'):
            rv=saxkit.IgnoringParser(False)
        elif name == (None, u'group'):
            rv=Group()
        else:
            raise "No parser from Comps for " + `name`
        return rv

    def addChild(self, name, child):
        if not isinstance(child, saxkit.IgnoringParser):
            self.groups[child.id]=child

    def getDefaultGroups(self):
        # return [self.groups[x] for x in ['core', 'base']]
        return [g for g in self.groups.values() if g.default == 'true']

    def __repr__(self):
        return "<Comps with %d groups>" % len(self.groups)

class Group(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.parsers[(None, 'id')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'name')]=saxkit.IgnoringParser(False)
        self.parsers[(None, 'description')]=saxkit.IgnoringParser(False)
        self.parsers[(None, 'uservisible')]=saxkit.IgnoringParser(False)
        self.parsers[(None, 'default')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'packagelist')]=PackageList()
        self.parsers[(None, 'langonly')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'biarchonly')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'grouplist')]=GroupList()

    def addChild(self, name, child):
        if isinstance(child, saxkit.SimpleValueParser):
            self.__dict__[name[1]]=child.getValue()
        elif isinstance(child, saxkit.SimpleListParser):
            self.__dict__[name[1]]=child.getValues()
        elif isinstance(child, PackageList):
            self.__dict__['packagelist']=child
        else:
            # Ignore anything else
            pass

    def __repr__(self):
        d=dict(self.__dict__)
        del d['parsers']
        return "<Group %s>" % `d`

class GroupList(saxkit.ElementHandler):

    def getParser(self, name):
        rv=None
        if name == (None, 'groupreq'):
            return saxkit.SimpleValueParser()
        elif name == (None, 'metapkg'):
            return saxkit.SimpleValueParser()
        else:
            raise "GroupList doesn't know how to parse child " + `name`
        return rv

class PackageList(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.mandatory=[]
        self.optional=[]
        self.default=[]

    def getParser(self, name):
        assert name[1] == 'packagereq'
        return saxkit.SimpleValueParser()

    def addChild(self, name, child):
        type=child.attrs[(None, 'type')]
        if type == 'default':
            self.default.append(child.getValue())
        elif type == 'optional':
            self.optional.append(child.getValue())
        elif type == 'mandatory':
            self.mandatory.append(child.getValue())
        else:
            raise "Unknown type:  " + child.attrs['type']

def parseComps(path):
    handler=saxkit.StackedHandler((None, 'comps'), Comps())
    parser=xml.sax.make_parser()
    parser.setFeature(xml.sax.handler.feature_namespaces, True)
    parser.setContentHandler(handler)
    parser.parse(path)
    return handler.getRootElement()

if __name__ == '__main__':
    c=parseComps(sys.argv[1])
    print c.groups['base']

    print "Default groups:", [g.id for g in c.getDefaultGroups()]
