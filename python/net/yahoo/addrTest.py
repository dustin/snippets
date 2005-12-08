#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 0D5A7D0A-4177-4050-B24A-F67B77E81E22

import sys
import ymap

if __name__ == '__main__':
    g=ymap.Geocoding("2wire-addrlookup")
    print g.lookupFree(sys.argv[1])
