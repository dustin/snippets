#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
arch-tag: 30A85C9B-1AE1-11D9-8753-00039359E8C6

Generate temperature readings for offline LEMP testing.
"""
import time
import socket
import random

therms=['1081841E000000DF', '10258D2A000000EA', '10C8892A00000096',
    '101D8A2A000000F7', '10E8C214000000E4', '1013A51E00000035', 'purple_mb',
    'purple_cpu', 'purple_chip']

rand=random.Random()

def mcast(msg):
    print "Multicasting: " + msg
    s=socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.sendto(msg, ('225.0.0.37', 6789))

def generate(therm):
    t=time.time()
    ts=time.strftime("%Y/%m/%d %H:%M:%S.0")
    reading=(rand.random() * 5) + 20.0
    msg=ts + "\t" + therm + "\t" + `reading`
    mcast(msg)

if __name__ == '__main__':
    while True:
        for therm in therms:
            generate(therm)
        time.sleep(60)
