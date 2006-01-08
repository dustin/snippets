#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 82F8C7E5-1799-4FD3-A3D3-0B703DB5EE0C

import unittest
import StringIO

import checkstat

class CLITest(unittest.TestCase):
    """Test the CLI thing."""

    def __runTest(self, expected, f, args):
        try:
            checkstat.runCheck(f, args, StringIO.StringIO())
        except SystemExit, e:
            self.assertEquals(expected, e[0])

    def testLowOK(self):
        "Test in-range with a low check."
        self.__runTest(0, lambda h,s: 100, ['-l', '50', '-L', '25', 'a', 'b'])

    def testLowWarn(self):
        "Test warning with a low check."
        self.__runTest(1, lambda h,s: 100, ['-l', '200', '-L', '25', 'a', 'b'])

    def testLowCrit(self):
        "Test critical with a low check."
        self.__runTest(2, lambda h,s: 100, ['-l', '200', '-L', '150', 'a', 'b'])

    def testHighOK(self):
        "Test in-range with a high check."
        self.__runTest(0, lambda h,s: 20, ['-h', '25', '-H', '50', 'a', 'b'])

    def testHighWarn(self):
        "Test warn with a high check."
        self.__runTest(1, lambda h,s: 30, ['-h', '20', '-H', '50', 'a', 'b'])

    def testHighCrit(self):
        "Test critical with a high check."
        self.__runTest(2, lambda h,s: 100, ['-h', '20', '-H', '50', 'a', 'b'])

    def testRangeOK(self):
        "Test in-range with a high and low check."
        self.__runTest(0, lambda h,s: 15,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeLowWarn(self):
        "Test warning on the low end with a high and low check."
        self.__runTest(1, lambda h,s: 7,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeLowCrit(self):
        "Test critical on the low end with a high and low check."
        self.__runTest(2, lambda h,s: 3,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeHighWarn(self):
        "Test warning on the high end with a high and low check."
        self.__runTest(1, lambda h,s: 30,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeHighCrit(self):
        "Test critical on the high end with a high and low check."
        self.__runTest(2, lambda h,s: 70,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

if __name__ == '__main__':
    unittest.main()
