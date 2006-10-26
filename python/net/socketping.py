#!/usr/bin/env python
"""
Test for any one of a bunch of sockets to work.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 6E8F62BD-EA65-4235-A316-1C5C73F865BE

import os
import sys
import time
import errno
import socket
import select
import traceback

def getSocket(host, port):
    s=socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.setblocking(False)
    # The connect may raise
    # socket.gaierror: (7, 'No address associated with nodename')
    rv = s.connect_ex((host, port))
    if not rv in [errno.EINPROGRESS]:
        raise socket.error(rv, os.strerror(rv))

    return s

def waitForThem(sockets, timeout, h):
    mysockets = list(sockets)
    found=[]
    finished = time.time() + timeout
    while mysockets != [] and time.time() < finished:
        r,w,e = select.select(mysockets, mysockets, mysockets, timeout)
        # If a socket errored, just remove it
        for s in e:
            mysockets.remove(s)
        # If a socket is readable, try to read from it (may just be waiting to
        # send us an error).
        for s in r:
            try:
                c=s.recv(1)
                print "Found %s:%d" % h[s]
                found.append(s)
            except socket.error, e:
                pass
            mysockets.remove(s)
        # If a socket is writable (and hasn't been removed already),
        # try to write to it.
        for s in w:
            if s in mysockets:
                try:
                    if s.send("x") == 1:
                        print "Found %s:%d" % h[s]
                        found.append(s)
                except socket.error, e:
                    pass
                mysockets.remove(s)

        # If we got nothing here, we timed out
        if r == w == e == []:
            mysockets=[]

    return found

def main():
    rv=0
    timeout=float(sys.argv[1])
    stuff=[x.split(':') for x in sys.argv[2:]]
    h={}
    sockets=[]
    for a, b in stuff:
        s=getSocket(a, int(b))
        h[s]=(a, int(b))
        sockets.append(s)

    # sockets=[getSocket(a, int(b)) for a, b in stuff]
    found=waitForThem(sockets, timeout, h)

    return rv

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage:  %s timeout host:port [host:port] [...]" % sys.argv[0]
        sys.exit(3)

    rv=0
    try:
        rv=main()
    except:
        sys.stdout.write(
            traceback.format_exception_only(sys.exc_type, sys.exc_value)[0])
        traceback.print_exc()
        rv=3

    sys.exit(rv)
