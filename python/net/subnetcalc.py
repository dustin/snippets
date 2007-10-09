#!/usr/bin/env python
"""
Simple network calculator functions.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys
import socket
import struct

def makeMask(bitlen):
    """Generate a mask at the given bit length."""
    s = "1" * bitlen
    while len(s) < 32:
        s += "0"
    rv=int(s, 2)
    return rv

def makeAddr(i):
    """Make an address out of the given integer representation of an IP"""
    return socket.inet_ntoa(struct.pack(">L", i))

def parseAddr(x):
    """Parse an IP address into an integer."""
    return int(struct.unpack(">L", socket.inet_aton(x))[0])

def getNetwork(x, mask):
    """Calculate the network address for the given IP with the given mask."""
    rv = parseAddr(x) & makeMask(mask)
    return makeAddr(rv)

if __name__ == '__main__':
    masks=range(33)
    masks.reverse()
    for mask in masks:
        print "%s/%d (%s)" % (getNetwork(sys.argv[1], mask),
            mask, makeAddr(makeMask(mask)))
