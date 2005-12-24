#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: C464DAC8-491E-4719-B1AD-E78227CF9115

import sys
import csv

import ymap

def getLonLat(addr, city, state):
    y=ymap.Geocoding("2wire-addrlookup")
    addrs=y.lookup(addr, city, state, "")
    if len(addrs) > 1:
        sys.stderr.write("Too many results:  " + `addrs` + "\n")
    return addrs[0].longitude, addrs[0].latitude

if __name__ == '__main__':
    f=open(sys.argv[1])
    reader=csv.reader(f.readlines())
    writer=csv.writer(sys.stdout)
    for r in reader:
        addr, city, state=r[2:5]
        lon, lat=getLonLat(addr, city, state)
        writer.writerow(r + [`lon`, `lat`])

    f.close()
