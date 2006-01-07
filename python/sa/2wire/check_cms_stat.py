#!/usr/bin/env python
"""
Check that the value of a particular stat is within an acceptable range.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 0A43B945-5161-4ED4-AC65-6F2BD700F2A6

import cmsstat
import checkstat

if __name__ == '__main__':
    def getVal(host, service):
        url="http://%s:8080/admin/monitor/stat" % (host,)
        return cmsstat.StatFetcher(url).getValue(service)

    checkstat.runCheck(getVal)
