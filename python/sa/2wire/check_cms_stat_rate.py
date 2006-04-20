#!/usr/bin/env python
"""
Check that the value of a particular stat is within an acceptable range.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 8D1FC56D-DEC7-49FA-9525-C9A1FA2B455F

import cmsstat
import checkstat

def getVal(host, service, default):
    url="http://%s:8080/admin/monitor/stat" % (host,)

    val=cmsstat.StatFetcher(url).getValue(service, default)
    storedVal=cmsstat.ArchivedStat(host, service)
    # make sure we store a value for this
    storedVal.store(val)

    oldval, adjust=storedVal.getValue(), 1.0
    if oldval is None:
        oldval=0
    else:
        adjust=storedVal.getAge()/60.0
    rate=float(val-oldval)/adjust

    return rate

if __name__ == '__main__':
    checkstat.runCheck(getVal)
