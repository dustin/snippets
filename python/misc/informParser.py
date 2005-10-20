#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 0189CC85-D42F-4EAE-9C77-EE95AB5E7925

import sys
import xml.sax

SOAPENV=u'http://schemas.xmlsoap.org/soap/envelope/'
CWMP=u'urn:dslforum-org:cwmp-1-0'

class ElementHandler(xml.sax.handler.ContentHandler):
    """Interface for element parsing."""

    parsers={}

    def __init__(self):
        xml.sax.handler.ContentHandler.__init__(self)

    def getParser(self, name):
        """Get the child parser for the given name"""
        return self.parsers[name]

    def addChild(self, name, child):
        """Add a parsed child object."""

class RootHandler(ElementHandler):
    element=None
    def __init__(self, type, handler):
        ElementHandler.__init__(self)
        self.parsers[type]=handler

    def addChild(self, name, child):
        assert self.element is None
        self.element=child

class StackedHandler(xml.sax.handler.ContentHandler):
    """SAX event handler that will delegate results to sub handlers."""
    stack=[]
    root=None

    def __init__(self, rootType, rootHandler):
        """root type and handler expected."""

        xml.sax.handler.ContentHandler.__init__(self)
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

#
# Begin implementation of SOAP parsing
#

class SoapMessage(ElementHandler):

    header=None
    body=None

    def __init__(self):
        ElementHandler.__init__(self)
        self.parsers[(SOAPENV, u'Header')]=SoapHeader()
        self.parsers[(SOAPENV, u'Body')]=SoapBody()

    def addChild(self, name, child):
        if name == (SOAPENV, u'Header'):
            self.header=child
        elif name == (SOAPENV, u'Body'):
            self.body=child
        else:
            raise "Invalid part", name

class HeaderItem(ElementHandler):
    name=None
    attrs={}
    content=""

    def startElementNS(self, name, qname, attrs):
        self.name=name
        for n in attrs.getQNames():
            self.attrs[attrs.getNameByQName(n)]=attrs.getValueByQName(n)

    def characters(self, content):
        self.content+=content

    def __repr__(self):
        return "{HeaderItem name=" + `self.name` + ", attrs=" + `self.attrs` \
            + ", content=" + `self.content` + "}"

class SoapHeader(ElementHandler):
    headers=[]
    def getParser(self, item):
        return HeaderItem()

    def addChild(self, name, child):
        self.headers.append(child)

    def __repr__(self):
        return "{Header " + `len(self.headers)` + " header(s) found}"

class SimpleValueParser(ElementHandler):
    value=""

    def characters(self, content):
        self.value+=content

    def getValue(self):
        return self.value

    def __repr__(self):
        return "{SimpleValue " + `self.value` + "}"

class DeviceId(ElementHandler):
    manufacturer=None
    oui=None
    productClass=None
    serialNumber=None

    def __init__(self):
        ElementHandler.__init__(self)
        self.parsers[(None, 'Manufacturer')]=SimpleValueParser()
        self.parsers[(None, 'OUI')]=SimpleValueParser()
        self.parsers[(None, 'ProductClass')]=SimpleValueParser()
        self.parsers[(None, 'SerialNumber')]=SimpleValueParser()

    def addChild(self, name, val):
        if name == (None, 'Manufacturer'):
            self.manufacturer=val.getValue()
        elif name == (None, 'OUI'):
            self.oui=val.getValue()
        elif name == (None, 'ProductClass'):
            self.productClass=val.getValue()
        elif name == (None, 'SerialNumber'):
            self.serialNumber=val.getValue()
        else:
            raise "WTF?"

    def __repr__(self):
        return "{DeviceId mfr=" + `self.manufacturer` \
            + ", oui=" + `self.oui` \
            + ", prodClass=" + `self.productClass` \
            + ", sn=" + `self.serialNumber` \
            + "}"

class Event(ElementHandler):
    eventCode=None
    commandKey=None

    def __init__(self):
        ElementHandler.__init__(self)
        self.parsers[(None, 'EventCode')]=SimpleValueParser()
        self.parsers[(None, 'CommandKey')]=SimpleValueParser()

    def addChild(self, name, val):
        shortName=name[1]
        if shortName == 'EventCode':
            self.eventCode=val.getValue()
        elif shortName == 'CommandKey':
            self.commandKey=val.getValue()

    def __repr__(self):
        return "{Event '" + self.eventCode + "', cmd='" \
            + self.commandKey + "'}"

class EventList(ElementHandler):
    events=[]
    def __init__(self):
        ElementHandler.__init__(self)
        self.parsers[(None, 'EventStruct')]=Event

    def getParser(self, name):
        # This one works a little differently...needs to return a new instance
        return self.parsers[name]()

    def addChild(self, name, val):
        self.events.append(val)

    def __repr__(self):
        return "{Events: " + `self.events` + "}"

class Parameters(ElementHandler):

    params={}

    def __init__(self):
        ElementHandler.__init__(self)

        class ParamValStruct(ElementHandler):
            name=None
            value=None
            def __init__(self):
                ElementHandler.__init__(self)
                self.parsers[(None, 'Name')]=SimpleValueParser()
                self.parsers[(None, 'Value')]=SimpleValueParser()

            def addChild(self, name, val):
                shortName=name[1]
                if shortName=='Name':
                    self.name=val.getValue()
                elif shortName=='Value':
                    self.value=val.getValue()
                else:
                    raise "WTF?"

        self.parsers[(None, 'ParameterValueStruct')]=ParamValStruct

    def getParser(self, name):
        # This one works a little differently...needs to return a new instance
        return self.parsers[name]()

    def addChild(self, name, val):
        self.params[val.name]=val.value

class Inform(ElementHandler):
    parts={}
    def __init__(self):
        ElementHandler.__init__(self)
        self.parsers[(None, 'DeviceId')]=DeviceId()
        self.parsers[(None, 'Event')]=EventList()
        self.parsers[(None, 'MaxEnvelopes')]=SimpleValueParser()
        self.parsers[(None, 'CurrentTime')]=SimpleValueParser()
        self.parsers[(None, 'RetryCount')]=SimpleValueParser()
        self.parsers[(None, 'ParameterList')]=Parameters()

    def addChild(self, name, val):
        if isinstance(val, SimpleValueParser):
            self.parts[name[1]]=val.getValue()
        else:
            self.parts[name[1]]=val

class BodyParserRegistry(object):
    parsers={(CWMP, u'Inform'): Inform}

    def getParser(self, name):
        return self.parsers[name]

# Singleton body parser
bodyParser=BodyParserRegistry()

class SoapBody(ElementHandler):
    body=None

    def addChild(self, name, child):
        assert self.body is None
        self.body=child

    def getParser(self, item):
        global bodyParser
        parserClass=bodyParser.getParser(item)
        return parserClass()

if __name__ == '__main__':
    handler=StackedHandler((SOAPENV, u'Envelope'), SoapMessage())
    parser=xml.sax.make_parser()
    parser.setFeature(xml.sax.handler.feature_namespaces, True)
    parser.setContentHandler(handler)
    parser.parse(sys.argv[1])

    soapMessage=handler.getRootElement()
    print "Header", soapMessage.header
    print "   Headers", soapMessage.header.headers
    print "Body", soapMessage.body.body

    informArgs=('DeviceId', 'Event', 'MaxEnvelopes', 'CurrentTime',
        'RetryCount')

    for k in informArgs:
        print k, "=", soapMessage.body.body.parts[k]

    print soapMessage.body.body.parts['ParameterList'].params
