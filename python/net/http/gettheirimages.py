#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""

import os
import sys
import md5
import sets
import time
import urllib

def getExtension(mimeType):
    m={'x-png': 'png', 'jpeg': 'jpg'}
    assert mimeType.startswith("image/")
    base=mimeType[6:]
    if m.has_key(base):
        base = m[base]
    return "." + base

def record(fn, data):
    if os.path.exists(fn):
        print fn + " already exists"
    else:
        print "Saving", fn
        tmp=fn + ".tmp"
        f=open(tmp, "w")
        f.write(data)
        f.close()

        os.rename(tmp, fn)

if __name__ == '__main__':
    seen=sets.Set()
    while True:
        f=urllib.urlopen(sys.argv[2])
        data=f.read()
        m=md5.md5(data)
        fn=os.path.join(sys.argv[1],
            m.hexdigest() + getExtension(f.headers.getheader("content-type")))
        f.close()

        record(fn, data)

        time.sleep(5)
