#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""

import sys
import time
import urllib2

class StatusHandler(urllib2.HTTPDefaultErrorHandler):
    def http_error_default(self, req, fp, code, msg, headers):
        result=urllib2.HTTPError(req.get_full_url(), code, msg, headers, fp)
        result.code=code
        return result

def getStatus(u):
    opener=urllib2.build_opener(StatusHandler())
    req=urllib2.Request(u)
    f=opener.open(req)
    rv=f.code
    f.close()
    return rv

if __name__ == '__main__':
    u=sys.argv[1]
    status=getStatus(u)
    while True:
        print "Got %d at %s" % (status, time.ctime())
        sys.stdout.flush()
        time.sleep(60)
        status=getStatus(u)
