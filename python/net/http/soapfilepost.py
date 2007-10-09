#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import sys
import urllib2
import traceback
try:
    import cookielib
    cookieJar = cookielib.CookieJar()
    cookieProcessor=urllib2.HTTPCookieProcessor(cookieJar)
    openerFactory = urllib2.build_opener
except ImportError:
    import ClientCookie
    cookieJar = ClientCookie.MozillaCookieJar()
    cookieProcessor=ClientCookie.HTTPCookieProcessor(cookieJar)
    openerFactory = ClientCookie.build_opener

class ErrorHandler(urllib2.HTTPDefaultErrorHandler):
    def http_error_default(self, req, fp, code, msg, hdrs):
        print "*** Got an error %d ***" % (code, )
        # print self, req, fp, code, msg, headers
        return fp

if __name__ == '__main__':

    headers={'SOAPAction': 'Inform', 'Content-type': 'text/xml'}
    url=sys.argv[1]
    opener=openerFactory(ErrorHandler(), cookieProcessor)

    for fn in sys.argv[2:]:
        sys.stderr.write("*** Sending " + fn + "\n")
        f=open(fn)
        req=urllib2.Request(sys.argv[1], f.read(), headers)
        f.close()
        response=opener.open(req)
        sys.stdout.writelines(response.info().headers)
        print ""
        print response.read()
