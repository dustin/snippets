#!/usr/bin/env python
"""
Testing for the postfix module.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 571B0986-3394-11D9-A419-000A957659CC

from __future__ import generators

import time
import UserList
import unittest
import postfix

class DummyPolicy(postfix.PolicyEngine):
    """Dummy policy implementation."""

    def __init__(self):
        postfix.PolicyEngine.__init__(self)
        self.count=0

    def process(self, attributes):
        self.count += 1
        return postfix.PolicyResponse.DEFER_IF_PERMIT

class WriteSink:
    """Sink writes."""

    def write(self, x):
        pass

class WriteCapture(UserList.UserList):

    def write(self, x):
        self.append(x)

class PolicyTest(unittest.TestCase):

    def lineGenerator(self, lines, total=1):
        for i in xrange(total):
            for l in lines:
                yield l

    def setUp(self):
        lines=[]

        lines.append("request=smtpd_access_policy\n")
        lines.append("protocol_state=RCPT\n")
        lines.append("protocol_name=SMTP\n")
        lines.append("helo_name=some.domain.tld\n")
        lines.append("queue_id=8045F2AB23\n")
        lines.append("sender=foo@bar.tld")
        lines.append("recipient=bar@foo.tld\n")
        lines.append("client_address=1.2.3.4\n")
        lines.append("client_name=another.domain.tld\n")
        lines.append("instance=123.456.7\n")
        lines.append("sasl_method=plain\n")
        lines.append("sasl_username=you\n")
        lines.append("sasl_sender=\n")
        lines.append("size=12345\n")
        lines.append("\n")

        self.lines=lines

    def testDirectProcess(self):
        dp=DummyPolicy()
        dp.process({'a': 1, 'b': 2})

    def testPolicyRun(self):
        dp=DummyPolicy()
        dp.run(input=self.lines, output=WriteSink())

        self.assertEquals(1, dp.count)

    def testPolicyRuns(self):
        numtests=5
        dp=DummyPolicy()
        dp.run(input=self.lineGenerator(self.lines, numtests),
            output=WriteSink())

        self.assertEquals(numtests, dp.count)

    def testPolicyRunsWithResults(self):
        numtests=5
        l=WriteCapture([])
        dp=DummyPolicy()
        dp.run(input=self.lineGenerator(self.lines, numtests), output=l)

        self.assertEquals(numtests, len(l))

    def testGreylist(self):
        delay=2
        gp=postfix.GreylistPolicyEngine({}, delay=delay)
        l=WriteCapture()

        # The first run should defer
        gp.run(input=self.lines, output=l)
        self.assertEquals(l[0],
            "action=" + postfix.PolicyResponse.DEFER_IF_PERMIT + "\n\n")
        del l[0]
        # Validate a second run also defers
        gp.run(input=self.lines, output=l)
        self.assertEquals(l[0],
            "action=" + postfix.PolicyResponse.DEFER_IF_PERMIT + "\n\n")

        # Now wait long enough, and it should pass
        time.sleep(delay + 0.5)
        del l[0]
        gp.run(input=self.lines, output=l)
        self.assertEquals(l[0],
            "action=" + postfix.PolicyResponse.DUNNO + "\n\n")

if __name__ == '__main__':
    unittest.main()
