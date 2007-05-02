#!/usr/bin/env python
"""
Merge a bunch of images into a single directory.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import os
import sys

def copyFile(s, d, f):
    assert f[-4:].lower() == '.jpg'
    bn=f[:-4]
    print bn
    tmp=os.path.join(d, f)
    i=0
    while(os.path.exists(tmp)):
        print "   %s exists" % tmp
        tmp = os.path.join(d, bn + "_" + str(i) + ".jpg")
        i+=1
    print "%s -> %s" % (s, tmp)
    os.link(s, tmp)

if __name__ == '__main__':
    src, dest = sys.argv[1:]

    for d, dirs, files in os.walk(src):
        srcs=[os.path.join(d, f) for f in files]
        for f in files:
            s=os.path.join(d, f)
            copyFile(s, dest, f)
