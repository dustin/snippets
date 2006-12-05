#!/usr/bin/env python
"""
Run forever making sure some items are reachable.

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 3818E147-2C2B-4F7F-B5F4-25452690130A

import os
import sys
import time
import errno
import socket
import select
import syslog
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

def waitForThem(sockets, timeout):
    mysockets={}
    for s, h, p in sockets:
        mysockets[s]=(s,h,p)
    found=[]
    finished = time.time() + timeout
    while mysockets != {} and time.time() < finished:
        socks=mysockets.keys()
        r,w,e = select.select(socks, socks, socks, timeout)
        # If a socket errored, just remove it
        for s in e:
            del mysockets[s]
        # If a socket is readable, try to read from it (may just be waiting to
        # send us an error).
        for s in r:
            try:
                c=s.recv(1)
                found.append(mysockets[s])
            except socket.error, e:
                pass
            del mysockets[s]
        # If a socket is writable (and hasn't been removed already),
        # try to write to it.
        for s in w:
            if s in mysockets.keys():
                try:
                    if s.send("x") == 1:
                        found.append(mysockets[s])
                except socket.error, e:
                    pass
                del mysockets[s]

        # If we got nothing here, we timed out
        if r == w == e == []:
            mysockets={}

    return found

def findLiveSockets(timeout, socks):
    assert len(socks) > 1
    sockets=[]
    for a, b in socks:
        try:
            sockets.append((getSocket(a, int(b)), a, int(b)))
        except:
            msg=traceback.format_exception_only(
                sys.exc_type, sys.exc_value)[0] + "\n" + traceback.format_exc()
            syslog.syslog(syslog.LOG_CRIT, msg)

    assert len(sockets) > 1
    found=waitForThem(sockets, timeout)
    return found

def parseSockets(strings):
    stuff=[x.split(':') for x in strings]
    rv=[]
    for a, b in stuff:
        rv.append((a, int(b)))
    return rv

def runLoop(timeout, socks):
    rv=False
    try:
        found=findLiveSockets(timeout, socks)
        rv=len(found) > 0
        for f in found:
            f[0].close()
    except:
        msg=traceback.format_exception_only(sys.exc_type, sys.exc_value)[0] \
            + "\n" + traceback.format_exc()
        syslog.syslog(syslog.LOG_CRIT, msg)
    return rv

def main(timeout, script, socks):
    while 1:
        if runLoop(timeout, socks):
            syslog.syslog(syslog.LOG_DEBUG, "Everything's going so well")
        else:
            syslog.syslog(syslog.LOG_WARNING, "Running " + script)
            os.system(script)
        time.sleep(300.0)

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage:  %s timeout script host:port [host:port] [...]" \
            % sys.argv[0]
        sys.exit(3)

    timeout=float(sys.argv[1])
    script=sys.argv[2]
    socks=parseSockets(sys.argv[3:])

    if os.fork() == 0:
        syslog.openlog("netcheck", syslog.LOG_PID, syslog.LOG_DAEMON)
        rv=main(timeout, script, socks)
        syslog.closelog()

    sys.exit(0)
