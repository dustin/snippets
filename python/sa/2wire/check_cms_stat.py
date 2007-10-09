#!/usr/bin/env python
"""
Check that the value of a particular stat is within an acceptable range.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import cmsstat
import checkstat

if __name__ == '__main__':
    def getVal(host, service, default):
        url="http://%s:8080/admin/monitor/stat" % (host,)
        return cmsstat.StatFetcher(url).getValue(service, default)

    checkstat.runCheck(getVal)
