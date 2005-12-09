#!/usr/bin/env python
"""
Yahoo maps interface.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 593463EA-F71B-4B3D-85E6-C6A4A587D307

import sys
import urllib
import traceback
import exceptions
import xml.dom.minidom

class Address(object):
    "Address instances represent a specific address from the geocoding lookup"
    precision=None
    latitude=None
    longitude=None
    address=None
    city=None
    state=None
    zipcode=None
    country=None

    def __repr__(self):
        return "<Address " + `self.__dict__` + ">"

class LimitExceeded(exceptions.Exception):
    "Exception thrown when API service limits are exceeded."

class UnknownError(exceptions.Exception):
    "Exception thrown when an unknown error is returned from Yahoo."

class Geocoding(object):
    "Interface to Yahoo's Geocoding service."

    BASEURL="http://api.local.yahoo.com/MapsService/V1/geocode"

    def __init__(self, appId):
        self.appId=appId

    def lookup(self, street, city, state, zipcode):
        "Lookup an address by street, city, state, and zipcode"
        params=urllib.urlencode((('appid', self.appId),
            ('street', street), ('city', city), ('state', state),
            ('zip', zipcode)))
        f=urllib.urlopen(self.BASEURL + "?" + params)
        try:
            data=f.read()
        finally:
            f.close()
        return self._parseResponse(data)

    def lookupFree(self, location):
        "Lookup an address by a freeform string."
        params=urllib.urlencode((('appid', self.appId), ('location', location)))
        f=urllib.urlopen(self.BASEURL + "?" + params)
        try:
            data=f.read()
        finally:
            f.close()
        return self._parseResponse(data)

    def __t(self, el, n):
        rv = None
        els=el.getElementsByTagName(n)
        if len(els) > 0:
            c=els[0].firstChild
            if c is not None:
                rv=c.data
        return rv

    # parse the response, return a list of addresses.
    def _parseResponse(self, docString):
        rv=None
        try:
            document=xml.dom.minidom.parseString(docString)
            # Check for error
            if document.firstChild.nodeName == "Error":
                msg=self.__t(document, "Message")
                if msg == "limit exceeded":
                    raise LimitExceeded, msg
                else:
                    raise UnknownError, msg + ":  " + docString
            # Get results
            assert document.firstChild.nodeName == "ResultSet"
            rv=[]
            for r in document.getElementsByTagName("Result"):
                addr=Address()
                addr.precision=r.getAttribute("precision")
                addr.latitude=float(self.__t(r, "Latitude"))
                addr.longitude=float(self.__t(r, "Longitude"))
                addr.address=self.__t(r, "Address")
                addr.city=self.__t(r, "City")
                addr.state=self.__t(r, "State")
                addr.zipcode=self.__t(r, "Zip")
                addr.country=self.__t(r, "Country")
                rv.append(addr)
        except LimitExceeded, e:
            raise e
        except UnknownError, e:
            raise e
        except:
            sys.stderr.write("Unhandled response:\n%s\n" % (docString, ))
            traceback.print_exc()
            raise sys.exc_info()
        return rv
