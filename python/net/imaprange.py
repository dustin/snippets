#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 9D0DD0F0-A659-11D9-ADE4-000A957659CC

from __future__ import generators
import unittest

def rangemaker(chunksize, maxnums):
    """Imap style rane generator"""
    assert chunksize > 0
    for i in xrange(0, maxnums, chunksize):
        max=i+(chunksize-1)
        if max > maxnums:
            max=maxnums
        rv = str(i) + ':' + str(max)
        yield rv

class RangeTest(unittest.TestCase):

    def checkExpected(self, expected, got):
        a=[]
        for i in got:
            a.append(i)
        self.assertEqual(expected, a)

    def testSingle(self):
        expected=['0:0', '1:1', '2:2', '3:3', '4:4',
            '5:5', '6:6', '7:7', '8:8', '9:9']
        ranges=rangemaker(1, 10)
        self.checkExpected(expected, ranges)

    def testByTen(self):
        expected=['0:9', '10:19', '20:29', '30:39', '40:49',
            '50:59', '60:69', '70:79', '80:89', '90:99']
        ranges=rangemaker(10, 100)
        self.checkExpected(expected, ranges)

    def testByTenLeftovers(self):
        expected=['0:9', '10:19', '20:29', '30:39', '40:49',
            '50:59', '60:69', '70:79', '80:89', '90:99', '100:109']
        ranges=rangemaker(10, 109)
        self.checkExpected(expected, ranges)

    def testSmall(self):
        expected=['0:3']
        ranges=rangemaker(10, 3)
        self.checkExpected(expected, ranges)

    def testZero(self):
        expected=[]
        ranges=rangemaker(1, 0)
        self.checkExpected(expected, ranges)

if __name__ == '__main__':
    unittest.main()
