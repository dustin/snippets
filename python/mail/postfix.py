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

class PolicyEngine:
    """Superclass for all postfix policy engines."""

    def __init__(self):
        """Initialize a PolicyEngine"""

        self.log=logging.getLogger("PolicyEngine")

    def process(self, attributes):
        """Process the given attributes and return a valid response or None if
           the you wish to declare that you don't care about the response."""
        raise NotImplemented

    def run(self, input=sys.stdin, output=sys.stdout):
        """Run the PolicyEngine reading from input and writing to output.
           input should be an iterable object producing lines (i.e. sys.stdin)
           and output should have a write() method (i.e. sys.stdout)."""

        attrs={}
        for l in input:
            l=l.rstrip()
            if l == '':
                # Empty line...we need to prep a response
                response=self.process(attrs)
                if response is None:
                    response = PolicyResponse.DUNNO
                self.log.debug("Response is " + response)
                output.write("action=" + response + "\n\n")
                attrs={}
            else:
                # Just another thing to parse
                parts=l.split('=', 2)
                if len(parts) != 2:
                    self.log.warn("Invalid/unexpected input line: " + l)
                else:
                    attrs[parts[0]]=parts[1]

class GreylistPolicyEngine(PolicyEngine):
    """PolicyEngine implementation that implements a greylist."""

    def __init__(self, db, delay=60):
        """Initialize with a db reference.
           db is an object that responds to __getitem__ and __setitem__.  No
           locking is performed on either of those routines, so if it's
           necessary, the db itself should implement it.
        """
        PolicyEngine.__init__(self)

        self.db=db
        self.delay=delay

    def getAge(self, key):
        """Get the age of this key in seconds.
           None if we haven't seen this key."""
        rv = None
        if key in self.db:
            now = time.time()
            oldval = float(self.db[key])
            rv = now - oldval
        return rv

    def storeKey(self, key):
        """Store the given key."""
        self.db[key] = str(time.time())

    def process(self, attributes):
        rv = None
        # Calculate a key from these fields
        keys=['client_address', 'sender', 'recipient']
        key = "/".join([attributes[k] for k in keys])

        age=self.getAge(key)
        if age is None or age < self.delay:
            rv = PolicyResponse.DEFER_IF_PERMIT
            self.storeKey(key)

        return rv
