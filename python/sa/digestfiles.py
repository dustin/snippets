#!/usr/bin/env python
"""
Create a list of md5s for all the files in a directory.
"""

from __future__ import with_statement

import os
import sys
import hashlib

BUFSIZE = 1024 * 1024

if __name__ == '__main__':
    for d, ds, files in os.walk(sys.argv[1]):
        for fn in files:
            fn = os.path.join(d, fn)
            dig = hashlib.md5()

            with open(fn) as f:
                data = f.read(BUFSIZE)
                while data:
                    dig.update(data)
                    data = f.read(BUFSIZE)
            print "%s\t%s" % (dig.hexdigest(), fn)
