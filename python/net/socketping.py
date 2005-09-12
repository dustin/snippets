#!/usr/bin/env python
"""
Test for any one of a bunch of sockets to work.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 6E8F62BD-EA65-4235-A316-1C5C73F865BE

import os
import sys
import errno
import socket
import select

def getSocket(host, port):
    s=socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setblocking(False)
    # The connect may raise
    # socket.gaierror: (7, 'No address associated with nodename')
    rv = s.connect_ex((host, port))
    if not rv in [errno.EINPROGRESS]:
        raise socket.error(rv, os.strerror(rv))

    return s

def waitForIt(sockets, timeout):
    foundOne=False
    mysockets = list(sockets)
    while mysockets != [] and not foundOne:
        r,w,e = select.select(mysockets, mysockets, mysockets, timeout)
        # If a socket errored, just remove it
        for s in e:
            mysockets.remove(s)
        # If a socket is readable, try to read from it (may just be waiting to
        # send us an error).
        for s in r:
            try:
                c=s.recv(1)
                foundOne=True
            except socket.error, e:
                mysockets.remove(s)
        # If a socket is writable (and hasn't been removed already),
        # try to write to it.
        for s in w:
            if s in mysockets:
                try:
                    s.send("x")
                    foundOne=True
                except socket.error, e:
                    mysockets.remove(s)

        # If we got nothing here, we timed out
        if r == w == e == []:
            mysockets=[]

    return foundOne

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage:  %s timeout host:port [host:port] [...]" % sys.argv[0]
        sys.exit(2)

    timeout=float(sys.argv[1])
    stuff=[x.split(':') for x in sys.argv[2:]]

    sockets=[getSocket(a, int(b)) for a, b in stuff]
    if waitForIt(sockets, timeout):
        print ":) Got a response."
        sys.exit(0)
    else:
        print ":( no hosts responded in %.2f seconds" % timeout
        sys.exit(2)
