#!/usr/bin/env python
"""
Make a cluster hosts file from a resin URL.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import re
import sys
import urllib2

def fetchHosts(url):
    """Get the hosts discovered via the given resin status URL."""
    pattern=re.compile(".*>\d+\..(.*):\d+")
    rv=[]
    f=urllib2.urlopen(url)
    for line in f.readlines():
        g=pattern.match(line)
        if g is not None:
            a=g.groups()[0].split('.')
            # Remove the last two parts of the host name (i.e. 2wire.com)
            host='.'.join(a[:-2])
            if host not in rv:
                rv.append(host)
    f.close()
    return rv

if __name__ == '__main__':
    print "Cluster " + " ".join(sys.argv[2:])
    for h in fetchHosts(sys.argv[1]):
        print "\t" + h
