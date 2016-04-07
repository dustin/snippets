#!/usr/bin/env python

import os
import sys

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

IN = os.fdopen(sys.stdin.fileno(), 'rb', 32)

def next():
    return ord(IN.read(1))

while True:
    chans = []
    n = next()
    if n == 0xa8:
        allbytes = [n]
        hdr1 = next()
        allbytes.append(hdr1)
        nchan = next()
        allbytes.append(nchan)
        for i in range(nchan):
            h = next()
            l = next()
            allbytes.extend([h, l])
            chans.append(((h * 256) + l) / 8)
        c = (next()*256) + next()

        if c != crc(allbytes):
            print "CRC ERROR:", allbytes, c, "\n"
        print "%s,%s\r" % (hdr1, ','.join(str(s) for s in chans))
