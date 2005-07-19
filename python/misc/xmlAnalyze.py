#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 110CE6DA-8A8B-49E6-B988-60E6EDA592A9

import sys
import getopt
import xml.sax

class CountingHandler(xml.sax.handler.ContentHandler):
    def __init__(self, wantAttrs=False, maxDepth=99999999):
        xml.sax.handler.ContentHandler.__init__(self)
        self.objects={}
        self.location=[]
        self.wantAttrs=wantAttrs
        self.maxDepth=maxDepth

    def startElement(self, name, attrs):
        # Default to just using the name
        nm=name
        if(self.wantAttrs):
            # Unless we want attribute names to uniquely identify paths, too
            ak=attrs.keys()
            ak.sort()
            nm=name + "[" + ','.join(ak) + "]"

        self.location.append(nm)

        # Only add the object if its depth is short enough
        if len(self.location) <= self.maxDepth:
            key='/'.join(self.location)
            self.objects[key] = self.objects.get(key, 0) + 1

    def endElement(self, name):
        self.location.pop()

if __name__ == '__main__':
    # Arbitrarily long value
    maxDepth=888
    showAttributes=False

    opts, args=getopt.getopt(sys.argv[1:], 'ad:')

    for o,v in opts:
        if o == '-a':
            showAttributes=True
        elif o == '-d':
            maxDepth=int(v)

    for f in args:
        print f
        handler=CountingHandler(showAttributes, maxDepth)
        xml.sax.parse(f, handler)

        keys=handler.objects.keys()
        keys.sort()
        for k in keys:
            print "\t" + k, handler.objects[k]
