#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
arch-tag: F5D13F18-1A3E-11D9-8DEA-000A957659CC
"""
import time
import socket

def sockLines(host, port):
    s=socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, port))
    f=s.makefile('rb')

    rv=f.readlines()

    f.close()
    s.close()

    return rv

def getTemps(host, port):
    lines=sockLines(host, port)
    temps=lines[1][:lines[1].index(';')]

    temps=map(float, temps[7:].split(', '))

    return temps

def mcast(msg):
    # print "Multicasting: " + msg
    s=socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.sendto(msg, ('225.0.0.37', 6789))

def getAndReport():
    t=time.time()
    ts=time.strftime("%Y/%m/%d %H:%M:%S.0")
    names=['mb', 'cpu', 'chip']
    temps=getTemps('purple', 1984)
    for i in range(len(temps)):
        msg=ts + "\t" + "purple_" + names[i] + "\t" + `temps[i]`
        mcast(msg)

if __name__ == '__main__':
    getAndReport()
