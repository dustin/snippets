#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 934D2C45-1DDC-461F-B3E0-06C491AF142A

import sys
import time
import urllib, urllib2

class StatusHandler(urllib2.HTTPDefaultErrorHandler):
    def http_error_default(self, req, fp, code, msg, headers):
        result=urllib2.HTTPError(req.get_full_url(), code, msg, headers, fp)
        result.code=code
        return result

def getStatus(u, etag=[]):
    opener=urllib2.build_opener(StatusHandler())
    req=urllib2.Request(u)
    if len(etag) > 0:
        es=", ".join(["\"" + s + "\"" for s in etag])
        req.add_header('If-None-Match', es)
    f=opener.open(req)
    rv=f.code
    f.close()
    etag=f.info().getheader('ETag')
    print "Returning", rv, etag
    return rv, etag

if __name__ == '__main__':
    u=sys.argv[1]
    etags=sys.argv[2:]
    status, etag=getStatus(u, etags)
    if etag not in etags:
        etags.append(etag)
    status=304
    while status == 304:
        time.sleep(5)
        status, etag=getStatus(u, etags)

    print "Updated!"
