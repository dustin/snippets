#!/usr/bin/env python
"""
Digest a tree of files.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import os
import sys
import md5

BLOCKSIZE=8*1024*1024

def digest_file(fn):
    m=md5.new()
    f=open(fn)
    data=f.read(BLOCKSIZE)
    while data:
        m.update(data)
        data=f.read(BLOCKSIZE)
    f.close()

    print "%s\t%s" % (m.hexdigest(), fn)

if __name__ == '__main__':
    for root, dirs, files in os.walk(sys.argv[1]):
        for fn in [os.path.join(root, f) for f in files]:
            digest_file(fn)
