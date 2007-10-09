#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

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
        self.__runTest(0, lambda h,s,d: 100, ['-l', '50', '-L', '25', 'a', 'b'])

    def testLowWarn(self):
        "Test warning with a low check."
        self.__runTest(1, lambda h,s,d: 100, ['-l', '200', '-L', '25', 'a', 'b'])

    def testLowCrit(self):
        "Test critical with a low check."
        self.__runTest(2, lambda h,s,d: 100, ['-l', '200', '-L', '150', 'a', 'b'])

    def testHighOK(self):
        "Test in-range with a high check."
        self.__runTest(0, lambda h,s,d: 20, ['-h', '25', '-H', '50', 'a', 'b'])

    def testHighWarn(self):
        "Test warn with a high check."
        self.__runTest(1, lambda h,s,d: 30, ['-h', '20', '-H', '50', 'a', 'b'])

    def testHighCrit(self):
        "Test critical with a high check."
        self.__runTest(2, lambda h,s,d: 100, ['-h', '20', '-H', '50', 'a', 'b'])

    def testRangeOK(self):
        "Test in-range with a high and low check."
        self.__runTest(0, lambda h,s,d: 15,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeLowWarn(self):
        "Test warning on the low end with a high and low check."
        self.__runTest(1, lambda h,s,d: 7,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeLowCrit(self):
        "Test critical on the low end with a high and low check."
        self.__runTest(2, lambda h,s,d: 3,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeHighWarn(self):
        "Test warning on the high end with a high and low check."
        self.__runTest(1, lambda h,s,d: 30,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeHighCrit(self):
        "Test critical on the high end with a high and low check."
        self.__runTest(2, lambda h,s,d: 70,
            ['-L', '5', '-l', '10', '-h', '20', '-H', '50', 'a', 'b'])

    def testRangeOKFloat(self):
        "Test in-range float with a high and low check."
        self.__runTest(0, lambda h,s,d: 15.3,
            ['-L', '5', '-l', '15.2', '-h', '15.4', '-H', '50', 'a', 'b'])

    def testRangeLowWarnFloat(self):
        "Test low warning float with a high and low check."
        self.__runTest(1, lambda h,s,d: 15.3,
            ['-L', '5', '-l', '15.4', '-h', '15.5', '-H', '50', 'a', 'b'])

    def testRangeHighWarnFloat(self):
        "Test high warning float with a high and low check."
        self.__runTest(1, lambda h,s,d: 15.3,
            ['-L', '5', '-l', '15.1', '-h', '15.2', '-H', '50', 'a', 'b'])

    def testNoDefault(self):
        "Test run with no default"
        def f(h, s, d):
            assert d is None
            return 15.3
        self.__runTest(1, f,
            ['-L', '5', '-l', '15.1', '-h', '15.2', '-H', '50', 'a', 'b'])

    def testDefault(self):
        "Test run with a default"
        def f(h, s, d):
            self.assertEquals(15.3, d)
            return d
        self.__runTest(1, f,
            ['-L', '5', '-l', '15.1', '-h', '15.2', '-H', '50',
                '-D', '15.3', 'a', 'b'])

if __name__ == '__main__':
    unittest.main()
