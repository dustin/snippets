#!/usr/bin/env python

import sys

def parseLine():
    try:
        l = sys.stdin.readline()
        a = l.split(',')
        return int(a[1], 16)
    except:
        sys.exit(0)

def crc(a):
    val = 0
    for b in a:
        val = (val & 0xffff) ^ (b << 8)
        for i in range(8):
            if val & 0x8000 != 0:
                val = (val << 1) ^ 0x1021
            else:
                val = (val << 1)
    return val & 0xffff

sys.stdin.readline()
while True:
    chans = []
    n = parseLine()
    if n == 0xa8:
        allbytes = [n]
        hdr1 = parseLine()
        allbytes.append(hdr1)
        nchan = parseLine()
        allbytes.append(nchan)
        for i in range(nchan):
            h = parseLine()
            l = parseLine()
            allbytes.extend([h, l])
            chans.append(((h * 256) + l) / 8)
        c = (parseLine()*256) + parseLine()

        if c != crc(allbytes):
            print "CRC ERROR:", allbytes, c
        print hdr1, chans
