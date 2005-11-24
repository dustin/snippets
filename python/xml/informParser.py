#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 0189CC85-D42F-4EAE-9C77-EE95AB5E7925

import sys
import saxkit
import xml.sax
import unittest

SOAPENV=u'http://schemas.xmlsoap.org/soap/envelope/'
CWMP=u'urn:dslforum-org:cwmp-1-0'

#
# Begin implementation of SOAP parsing
#

class SoapMessage(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.header=None
        self.body=None
        self.parsers[(SOAPENV, u'Header')]=SoapHeader()
        self.parsers[(SOAPENV, u'Body')]=SoapBody()

    def addChild(self, name, child):
        if name == (SOAPENV, u'Header'):
            self.header=child
        elif name == (SOAPENV, u'Body'):
            self.body=child
        else:
            raise "Invalid part", name

class HeaderItem(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.name=None
        self.attrs={}
        self.content=""

    def startElementNS(self, name, qname, attrs):
        self.name=name
        for n in attrs.getQNames():
            self.attrs[attrs.getNameByQName(n)]=attrs.getValueByQName(n)

    def characters(self, content):
        self.content+=content

    def __repr__(self):
        return "{HeaderItem name=" + `self.name` + ", attrs=" + `self.attrs` \
            + ", content=" + `self.content` + "}"

class SoapHeader(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.headers=[]

    def getParser(self, item):
        return HeaderItem()

    def addChild(self, name, child):
        self.headers.append(child)

    def __repr__(self):
        return "{Header " + `len(self.headers)` + " header(s) found}"

class DeviceId(saxkit.ElementHandler):
    manufacturer=None
    oui=None
    productClass=None
    serialNumber=None

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.parsers[(None, 'Manufacturer')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'OUI')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'ProductClass')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'SerialNumber')]=saxkit.SimpleValueParser()

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

class Event(saxkit.ElementHandler):
    eventCode=None
    commandKey=None

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.parsers[(None, 'EventCode')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'CommandKey')]=saxkit.SimpleValueParser()

    def addChild(self, name, val):
        shortName=name[1]
        if shortName == 'EventCode':
            self.eventCode=val.getValue()
        elif shortName == 'CommandKey':
            self.commandKey=val.getValue()

    def __repr__(self):
        return "{Event '" + self.eventCode + "', cmd='" \
            + self.commandKey + "'}"

class EventList(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.parsers[(None, 'EventStruct')]=Event
        self.events=[]

    def getParser(self, name):
        # This one works a little differently...needs to return a new instance
        return self.parsers[name]()

    def addChild(self, name, val):
        self.events.append(val)

    def __repr__(self):
        return "{Events: " + `self.events` + "}"

class Parameters(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.params={}

        class ParamValStruct(saxkit.ElementHandler):
            name=None
            value=None
            def __init__(self):
                saxkit.ElementHandler.__init__(self)
                self.parsers[(None, 'Name')]=saxkit.SimpleValueParser()
                self.parsers[(None, 'Value')]=saxkit.SimpleValueParser()

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

class Inform(saxkit.ElementHandler):
    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.parsers[(None, 'DeviceId')]=DeviceId()
        self.parsers[(None, 'Event')]=EventList()
        self.parsers[(None, 'MaxEnvelopes')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'CurrentTime')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'RetryCount')]=saxkit.SimpleValueParser()
        self.parsers[(None, 'ParameterList')]=Parameters()

        self.parts={}

    def addChild(self, name, val):
        if isinstance(val, saxkit.SimpleValueParser):
            self.parts[name[1]]=val.getValue()
        else:
            self.parts[name[1]]=val

class BodyParserRegistry(object):
    parsers={(CWMP, u'Inform'): Inform}

    def getParser(self, name):
        return self.parsers[name]

# Singleton body parser
bodyParser=BodyParserRegistry()

class SoapBody(saxkit.ElementHandler):

    def __init__(self):
        saxkit.ElementHandler.__init__(self)
        self.body=None

    def addChild(self, name, child):
        assert self.body is None
        self.body=child

    def getParser(self, item):
        global bodyParser
        parserClass=bodyParser.getParser(item)
        return parserClass()

class InformTest(unittest.TestCase):

    def setUp(self):
        self.handler=saxkit.StackedHandler(
            (SOAPENV, u'Envelope'), SoapMessage())
        parser=xml.sax.make_parser()
        parser.setFeature(xml.sax.handler.feature_namespaces, True)
        parser.setContentHandler(self.handler)
        parser.parse("sample/informbootstrap.xml")
        self.sm=self.handler.getRootElement()

    def testInstances(self):
        self.failUnless(isinstance(self.sm, SoapMessage))
        self.failUnless(isinstance(self.sm.body, SoapBody))
        self.failUnless(isinstance(self.sm.header, SoapHeader))

    def testHeader(self):
        self.assertEquals(len(self.sm.header.headers), 1)

    def testBody(self):
        b=self.sm.body.body.parts
        self.assertEquals(b['DeviceId'].manufacturer, '%(manufacturer)s')
        self.assertEquals(b['DeviceId'].oui, '%(oui)s')
        self.assertEquals(b['DeviceId'].productClass, '%(pca)s')
        self.assertEquals(b['DeviceId'].serialNumber, '%(serial)s')

        self.assertEquals(len(b['Event'].events), 1)

        informArgs=('DeviceId', 'Event', 'MaxEnvelopes', 'CurrentTime',
            'RetryCount')

        # for k in informArgs:
        #     print k, "=", soapMessage.body.body.parts[k]

        # print soapMessage.body.body.parts['ParameterList'].params

if __name__ == '__main__':
    unittest.main()
