#!/usr/bin/env python
"""
Trying to get a DNS server working in twisted.

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

from twisted.application import internet, service
from twisted.internet import protocol, reactor, defer, utils
from twisted.protocols import dns
from twisted.names.server import DNSServerFactory
from twisted.names import client

class MyDNSServer(DNSServerFactory):
    """Factory for building finger instances"""

    def __init__(self, authorities = None, caches = None,
        clients = None, verbose = 0):

        r=DNSServerFactory.__init__(self, authorities, caches, clients, verbose)
        return r

    def connectionMade(self, protocol):
        print "Got a connection", protocol
        return DNSServerFactory.connectionMade(self, protocol)

    def handleQuery(self, message, protocol, address):
        print "Got", message.toStr(), protocol, address
        return DNSServerFactory.handleQuery(self, message, protocol, address)

if __name__ == '__main__':
    port=1053
    f=MyDNSServer([client.createResolver(servers=['192.168.1.40'])],
        verbose=True)
    p=dns.DNSDatagramProtocol(f)
    ret = service.MultiService()
    for (klass, arg) in [(internet.TCPServer, f), (internet.UDPServer, p)]:
        s = klass(port, arg)
        s.setServiceParent(ret)
    ret.startService()
    reactor.run()
