#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 2404B2FA-AAAD-4ABC-BF05-48CFD680FC03

import os
import sys
import stat

def sizeup(n):
    ext=' kmgt'
    extp=0
    while n > 1024:
        extp += 1
        n = float(n) / 1024.0
    return "%.2f%s" % (n, ext[extp])

if __name__ == '__main__':
    numfiles=0
    total=0
    for d, ds, files in os.walk(sys.argv[1]):
        numfiles += len(files)
        for f in files:
            total += os.stat(os.path.join(d, f))[stat.ST_SIZE]
    print numfiles, total, sizeup(total)
