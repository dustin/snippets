#!/usr/bin/env python
"""
Library for postfix helper programs.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 2E62701B-3390-11D9-B246-000A957659CC

import sys
import time
import logging

class PolicyResponse:
    """A collection of canned policy responses"""

    DUNNO="dunno"

    DEFER_IF_PERMIT="defer_if_permit Service temporarily unavailable"

class PolicyEngine(object):
    """Superclass for all postfix policy engines."""

    def __init__(self):
        """Initialize a PolicyEngine"""

        self.log=logging.getLogger(self.__class__.__name__)

    def process(self, attributes):
        """Process the given attributes and return a valid response or None if
           the you wish to declare that you don't care about the response."""
        raise NotImplemented

    def run(self, input=sys.stdin, output=sys.stdout):
        """Run the PolicyEngine reading from input and writing to output.
           input should have a readline() method (i.e. sys.stdin)
           and output should have a write() method (i.e. sys.stdout)."""

        attrs={}
        # I have to read a line a time here in order to get stuff working
        l=input.readline()
        while l != '':
            l=l.rstrip()
            if l == '':
                # Empty line...we need to prep a response
                self.log.debug("Request is %s", attrs)

                response=self.process(attrs)
                if response is None:
                    response = PolicyResponse.DUNNO

                self.log.info("Response is %s", response)
                output.write("action=" + response + "\n\n")
                output.flush()
                # Reset the attributes for the next request
                attrs={}
            else:
                # Just another thing to parse
                # self.log.debug("Read line:  %s", l)
                parts=l.split('=', 1)
                if len(parts) != 2:
                    self.log.warn("Invalid/unexpected input line: %s", l)
                else:
                    attrs[parts[0]]=parts[1]
            # Read the next line
            l=input.readline()

class ChainedPolicyEngine(PolicyEngine):
    """PolicyEngine that executes multiple policy engine until one returns a
       value."""

    def __init__(self, engines=[]):
        PolicyEngine.__init__(self)
        self.engines=engines

    def addEngine(self, engine):
        """Add an engine to the processing chain."""
        self.engines.append(engine)

    def process(self, attributes):
        """Process all of the engines until a favorable result is returned."""
        rv = None
        for engine in self.engines:
            rv = engine.process(attributes)
            if rv is not None:
                self.log.info("Got a response from %s",
                    engine.__class__.__name__)
                break
        return rv

class GreylistPolicyEngine(PolicyEngine):
    """PolicyEngine implementation that implements a greylist."""

    def __init__(self, db, delay=60):
        """Initialize with a db reference.
           db is an object that responds to __getitem__ and __setitem__.  No
           locking is performed on either of those routines, so if it's
           necessary, the db itself should implement it.
        """
        PolicyEngine.__init__(self)

        self.log=logging.getLogger("GreylistPolicyEngine")

        self.db=db
        self.delay=delay

    def getRecord(self, key):
        rv = None
        if key in self.db:
            vs=self.db[key]
            sa=vs.split(' ')
            rv=(float(sa[0]), int(sa[1]))
        else:
            rv = (time.time(), 0)
        return rv

    def storeKey(self, key, count):
        """Store the given key."""
        self.db[key] = " ".join([str(time.time()), str(count+1)])

    def process(self, attributes):
        rv = None
        try:
            # Calculate a key from the first three octets of the sender IP
            # address, the sender email addr, and the recipient email addr
            subnet=".".join(attributes['client_address'].split(".")[:3])
            sender=attributes['sender']
            recip=attributes['recipient']
            key = "/".join([subnet, sender, recip])

            (t, count) = self.getRecord(key)
            age = time.time() - t

            if age < self.delay:
                self.log.info("Deferring %s: (age: %.2fs)", key, age)
                rv = PolicyResponse.DEFER_IF_PERMIT
            else:
                self.log.info("Permitting %s (age %.2f, count=%d)",
                    key, age, count)
            self.storeKey(key, count)
        except KeyError:
            self.log.exception("Could not process %s", attributes)

        return rv
