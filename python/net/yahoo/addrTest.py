#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys
import ymap

if __name__ == '__main__':
    g=ymap.Geocoding("2wire-addrlookup")
    print g.lookupFree(sys.argv[1])
