#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: A86364A4-F973-4AF2-A4A5-5FDCA7F37D82

import unittest
import ymap

class TestGeocoding(unittest.TestCase):
    "Geocoding API client test."

    def getFileData(self, p):
        f=open(p)
        r=f.read()
        f.close()
        return r

    def testParsing(self):
        "Validate we can parse responses"
        g=ymap.Geocoding("x")
        r=g._parseResponse(self.getFileData("data/testAddrResponse.xml"))
        self.assertEquals(len(r), 1)
        addr=r[0]
        self.assertAlmostEqual(addr.latitude, 37.416384)
        self.assertAlmostEqual(addr.longitude, -122.024853)
        self.assertEquals(addr.address, "701 FIRST AVE")
        self.assertEquals(addr.city, "SUNNYVALE")
        self.assertEquals(addr.state, "CA")
        self.assertEquals(addr.zipcode, "94089-1019")
        self.assertEquals(addr.country, "US")

    def testExceededErrorParsing(self):
        "Validate we can parse limit exceeded errors"
        g=ymap.Geocoding("x")
        try:
            r=g._parseResponse(self.getFileData("data/testError.xml"))
            fail("Expected a LimitExceeded exception.")
        except ymap.LimitExceeded, e:
            pass

    def testUnknownErrorParsing(self):
        "Validate we can parse unknown errors"
        g=ymap.Geocoding("x")
        try:
            r=g._parseResponse(self.getFileData("data/testError2.xml"))
            fail("Expected an UnknownError exception.")
        except ymap.UnknownError, e:
            # Make sure we got the message
            self.assertEquals(e[0], "spanish inquisition")

if __name__ == '__main__':
    unittest.main()
