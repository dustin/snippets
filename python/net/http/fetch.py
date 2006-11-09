#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: A538F9C4-74FB-458C-8FFB-02768F789B50

import os
import sys
import shutil
import signal
import urllib2

class StatusHandler(urllib2.HTTPDefaultErrorHandler):
    def http_error_default(self, req, fp, code, msg, headers):
        result=urllib2.HTTPError(req.get_full_url(), code, msg, headers, fp)
        result.code=code
        return result

def getEtag(wantedfile):
    rv=None
    etf=wantedfile + ".etag"
    if os.path.exists(etf):
        f=open(etf)
        rv=f.read().strip()
        f.close()
    return rv

def saveEtag(wantedfile, etag):
    if etag is not None:
        etf=wantedfile + ".etag"
        tmpfile=etf + ".tmp"
        f=open(tmpfile, "w")
        f.write(etag)
        f.close()
        os.rename(tmpfile, etf)

def doUpdate(u, wantedfile):
    tmpfile=wantedfile + ".tmp"

    req=urllib2.Request(u)
    etag=getEtag(wantedfile)
    if etag is not None:
        req.add_header('If-None-Match', etag);

    opener=urllib2.build_opener(StatusHandler())
    i=opener.open(req)

    # This looks a little strange, but it gives me python 2.2 compatibility
    # If the code attr doesn't exist, or it does and it's 200, then we update.
    if not hasattr(i, 'code') or i.code == 200:
        o=open(tmpfile, "w")

        shutil.copyfileobj(i, o)
        os.rename(tmpfile, wantedfile)

        etag=i.info().getheader('ETag')
        saveEtag(wantedfile, etag)

if __name__ == '__main__':

    # You have thirty seconds to comply
    signal.alarm(30)

    wantedfile=sys.argv[2]
    doUpdate(sys.argv[1], wantedfile)
