#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: B9C25E5C-38E1-11D9-9AE5-000393CFE6B8

import os
import sys
import time
import sched
import random
import logging
import unittest

import simtime

class NoOffsetException:
    pass

class Gateway:

    def __init__(self, offset, timefunc):
        self.offset=offset
        self.timefunc=timefunc

    def getTime(self):
        """Get the local time of this device"""
        if self.offset is None:
            raise NoOffsetException()

        return (self.timefunc() + (self.offset * 3600))

class Schedule:

    def __init__(self, start, duration):
        self.start=start
        self.duration=duration

        self.r=random.Random()

    def getDelay(self, gw):
        rv = self.start - gw.getTime()
        # If this is yesterday, add a day to it
        while rv < 0:
            rv += 86400

        # Add a random offset
        rv += self.r.randint(0, self.duration)

        return rv

# Tests involving gateway time reporting
class GwTimeTest(unittest.TestCase):

    # Validate a gateway that doesn't know its time won't report it
    def testUnkownTime(self):
        gw=Gateway(None, time.time)
        try:
            gw.getTime()
            fail("I shouldn't know my time.")
        except NoOffsetException:
            pass

    # Real time tests
    def testRealTimePacific(self):
        gw=Gateway(0, time.time)
        self.assertAlmostEqual(gw.getTime(), time.time(), 1)

    def testRealTimeCentral(self):
        gw=Gateway(2, time.time)
        self.assertAlmostEqual(gw.getTime(), time.time() + 7200, 1)

    def testRealTimeHawaii(self):
        gw=Gateway(-2, time.time)
        self.assertAlmostEqual(gw.getTime(), time.time() - 7200, 1)

    # Simulated time tests
    def testSimTimePacific(self):
        ts=simtime.TimeSim(60)
        time.sleep(0.5)
        gw=Gateway(0, ts.time)
        self.assertAlmostEqual(gw.getTime(), ts.time(), 1)

    def testSimTimeCentral(self):
        ts=simtime.TimeSim(60)
        time.sleep(0.5)
        gw=Gateway(2, ts.time)
        self.assertAlmostEqual(gw.getTime(), ts.time() + 7200, 1)

    def testSimTimeHawaii(self):
        ts=simtime.TimeSim(60)
        time.sleep(0.5)
        gw=Gateway(-2, ts.time)
        self.assertAlmostEqual(gw.getTime(), ts.time() - 7200, 1)

# Tests involving actual scheduling
class GwSchedTest(unittest.TestCase):

    def __init__(self, name):
        unittest.TestCase.__init__(self, name)
        self.log=logging.getLogger("GwSchedTest")

    def testExactSchedPac(self):
        start=time.time()
        s=Schedule(start + 60, 0)
        gw=Gateway(0, time.time)

        self.assertNotAlmostEquals(0, s.getDelay(gw), 0)
        self.assertAlmostEqual(60, s.getDelay(gw), 0)
        self.assertNotAlmostEquals(120, s.getDelay(gw), 0)

    def testExactSchedCentralTooSoon(self):
        start=time.time()
        s=Schedule(start + 60, 0)
        gw=Gateway(2, time.time)

        self.assertAlmostEqual((86400 - (2 * 3600)) + 60, s.getDelay(gw), 0)

    def testExactSchedCentralNormal(self):
        start=time.time()
        # Going to run in about 5 hours
        s=Schedule(start + (5 * 3600), 0)
        gw=Gateway(2, time.time)

        # Two hours ago, five hours from now
        self.assertAlmostEqual(((5 * 3600) - (2 * 3600)), s.getDelay(gw), 0)

    def checkSched(self, gw, min, duration):
        max=min + duration
        ts = gw.getTime()

        self.log.debug("Checking %.2f < %.2f < %.2f", min, ts, max)

        # Round up the day
        if ts > max and (ts + 86400) > max:
            max += 86400
            self.log.debug(" bump  %.2f < %.2f < %.2f", min, ts, max)

        if ts > max:
            self.fail("timestamp is out of range: (%.2f > %.2f)" % (ts, max))
        if ts < min:
            self.fail("timestamp is out of range: (%.2f < %.2f)" % (ts, min))

    # Test a few devices on a schedule in pacific time
    def testSchedPac(self):
        # 1000x faster than real time
        ts=simtime.TimeSim(1000)
        start=ts.time()
        minstart = start + (1 * 3600)
        duration=3600
        delayer=Schedule(minstart, duration)

        gw=Gateway(0, ts.time)

        sleeptime = delayer.getDelay(gw)

        ts.sleep(sleeptime)
        self.checkSched(gw, minstart, duration)

    def severalTester(self, gwgen):
        # 1000x faster than real time
        ts=simtime.TimeSim(1000)
        start=ts.time()
        minstart = start + (1 * 3600)
        duration=3600
        delayer=Schedule(minstart, duration)

        s=sched.scheduler(ts.time, ts.sleep)

        # Generate about a hundred gateways and schedule them to fire
        for i in range(100):
            gw=gwgen(ts.time)
            delay=delayer.getDelay(gw)
            self.log.debug("Having gw sleep for %.2f seconds", delay)
            s.enter(delay, 1, self.checkSched, (gw, minstart, duration))

        s.run()

    def testSeveralSchedPac(self):
        self.log.debug("testSeveralSchedPac")
        def gwgen(t):
            return Gateway(0, t)
        self.severalTester(gwgen)

    def testSeveralSchedCentral(self):
        self.log.debug("testSeveralSchedCentral")
        def gwgen(t):
            return Gateway(2, t)
        self.severalTester(gwgen)

    def testSeveralSchedRandom(self):
        self.log.debug("testSeveralSchedRandom")
        r=random.Random()
        def gwgen(t):
            return Gateway(r.randint(-4, 19), t)
        self.severalTester(gwgen)

if __name__ == '__main__':
    # Logging config
    hdlr=logging.FileHandler(",test.log", "w")
    hdlr.setFormatter(logging.Formatter(logging.BASIC_FORMAT))
    logging.root.addHandler(hdlr)
    logging.root.setLevel(logging.DEBUG)

    unittest.main()
