#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

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

    def testParsing2(self):
        "Validate we can parse responses given a bad response I got"
        g=ymap.Geocoding("x")
        r=g._parseResponse(self.getFileData("data/testAddResponse2.xml"))
        self.assertEquals(len(r), 1)
        addr=r[0]
        self.assertAlmostEqual(addr.latitude, 29.5148)
        self.assertAlmostEqual(addr.longitude, -98.5235)
        self.failUnless(addr.address is None)
        self.assertEquals(addr.city, "SAN ANTONIO")
        self.assertEquals(addr.state, "TX")
        self.assertEquals(addr.zipcode, "78213")
        self.assertEquals(addr.country, "US")

    def testExceededErrorParsing(self):
        "Validate we can parse limit exceeded errors"
        g=ymap.Geocoding("x")
        try:
            r=g._parseResponse(self.getFileData("data/testError.xml"))
            self.fail("Expected a LimitExceeded exception, got: " + `r`)
        except ymap.LimitExceeded:
            pass

    def testUnknownErrorParsing(self):
        "Validate we can parse unknown errors"
        g=ymap.Geocoding("x")
        try:
            r=g._parseResponse(self.getFileData("data/testError2.xml"))
            self.fail("Expected an UnknownError exception, got: " + `r`)
        except ymap.UnknownError, e:
            # Make sure we got the message
            self.assertTrue(e[0].startswith("spanish inquisition"))

if __name__ == '__main__':
    unittest.main()
