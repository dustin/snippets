#!/usr/bin/env python
"""
Given True and False, NAND is the fundamental logic operation.

All other logic operations can be built upon it.  At least, that's what I've
been told.  Here's my attempt (written on my OLPC at the drive-in).

Copyright (c) 2008  Dustin Sallings <dustin@spy.net>
"""

import unittest

def nand(a, b):
    return not (a and b)

def lnot(a):
    return nand(a, True)

def land(a, b):
    return lnot(nand(a, b))

def lor(a, b):
    return nand(nand(a, True), nand(b, True))

def lxor(a, b):
    return land(lor(a, b), nand(a, b))

def print_all():
    print '-' * 40
    def p1(f, a):
        print "%s(%s) == %s" % (f.__name__, a, f(a))

    def p2(f, a, b):
        print "%s(%s, %s) == %s" % (f.__name__, a, b, f(a, b))

    def p2all(f):
        t=[(1, 1), (1, 0), (0, 1), (0, 0)]
        for pair in t:
            p2(f, *[x == 1 for x in pair])

    p2all(nand)

    p1(lnot, True)
    p1(lnot, False)

    for f in (land, lor, lxor):
        p2all(f)

class LogicTest(unittest.TestCase):

    def assertVals(self, f, *exp):
        t=[(1, 1), (1, 0), (0, 1), (0, 0)]
        for pair, e in zip(t, exp):
            v=f(*[x == 1 for x in pair])
            self.assertEquals(e, v, "Failure on %s%s, expected %s, got %s" %
                    (f.__name__, `pair`, e, v))

    def testNand(self):
        self.assertVals(nand, False, True, True, True)

    def testNot(self):
        self.assertEquals(True, lnot(False))
        self.assertEquals(False, lnot(True))

    def testAnd(self):
        self.assertVals(land, True, False, False, False)

    def testOr(self):
        self.assertVals(lor, True, True, True, False)

    def testXor(self):
        self.assertVals(lxor, False, True, True, False)

if __name__ == '__main__':
    unittest.main()
