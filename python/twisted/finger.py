#!/usr/bin/env python
"""
Twisted finger server example.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

from twisted.internet import protocol, reactor, defer, utils
from twisted.protocols import basic
from twisted.web import client

class FingerProtocol(basic.LineReceiver):
    """Finger protocol handler."""

    def __myResponseHandler(self, res):
        # parse and spit out the slimp3 response
        rv=""
        tmp=""
        state=0
        rs=chr(30)
        for x in res:
            if x == rs:
                if state == 0:
                    state = 1
                    if tmp == "linebreak":
                        rv = rv + "\r\n"
                    tmp=""
                else:
                    state = 0
            else:
                if state == 0:
                    rv = rv + x
                else:
                    tmp = tmp + x
        self.transport.write(rv + "\r\n")
        self.transport.loseConnection()

    def lineReceived(self, user):
        """What to do when a line comes in."""
        d=self.factory.getUser(user)
        d.addErrback(lambda x: "Error:  " + `x`)
        d.addCallback(self.__myResponseHandler)

class FingerFactory(protocol.ServerFactory):
    """Factory for building finger instances"""
    protocol = FingerProtocol

    def getUser(self, user):
        # return defer.succeed("No such user")
        # return utils.getProcessOutput("finger", [user])
        return client.getPage(
            "http://disk.west.spy.net:10110/status.txt?p0=status")

if __name__ == '__main__':
    reactor.listenTCP(1079, FingerFactory())
    reactor.run()
