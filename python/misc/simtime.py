#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import time
import unittest

class TimeSim:
    """Time simulation class."""

    def __init__(self, tmultiplier=1):
        self.start=time.time()
        self.tmult=tmultiplier

    def time(self):
        delta=time.time() - self.start
        delta = delta * self.tmult
        rv = self.start + delta

        return rv

    def sleep(self, delay):
        sdelay = (delay / self.tmult)
        time.sleep(sdelay)

class TimeSimTest(unittest.TestCase):

    def rtSleep(self, ts, delay, expect):
        start=ts.time()
        time.sleep(delay)
        end=ts.time()
        self.assertAlmostEqual(start + expect, end, 1)

    def simSleep(self, ts, delay, expect):
        start=time.time()
        ts.sleep(delay)
        end=time.time()
        self.assertAlmostEqual(start + expect, end, 1)

    def testRealtime(self):
        ts=TimeSim()

        # Validate sleeping for two seconds of real time on a time sim with a
        # multiplier of 1 gives me about two seconds of simulated time passed
        self.rtSleep(ts, 2, 2)

        # Similar to the above test, but use real time for timestamps and
        # simulated time for sleep
        self.simSleep(ts, 2, 2)

    def testDoubleTime(self):
        ts=TimeSim(2)
        self.rtSleep(ts, 2, 4)
        self.simSleep(ts, 2, 1)

    def testHalfTime(self):
        ts=TimeSim(0.5)
        self.rtSleep(ts, 2, 1)
        self.simSleep(ts, 2, 4)

if __name__ == '__main__':
    unittest.main()
