#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 0E5E491A-4313-11D9-A1C3-000393CFE6B8

import sys
import ClientCookie


if __name__ == '__main__':

    headers={'SOAPAction': 'Inform', 'Content-type': 'text/xml'}
    url=sys.argv[1]

    for fn in sys.argv[2:]:
        sys.stderr.write("*** Sending " + fn + "\n")
        f=open(fn)
        req=ClientCookie.Request(sys.argv[1], f.read(), headers)
        f.close()
        response=ClientCookie.urlopen(req)
        sys.stdout.writelines(response.info().headers)
        print ""
        print response.read()
