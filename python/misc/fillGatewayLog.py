#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: fillGatewayLog.py,v 1.1 2003/08/20 01:04:04 dustin Exp $
"""

import random
import sys
import Stats

r=random.Random()

def getOrgKey():
    return r.randint(36,123)

def getTxnTime():
    """Calculate a reasonable transaction time"""
    t=r.randint(0,100)
    rv=(1.1**t)+150
    return int(rv)

periodic=2012
onceTypes=(2000,2011,2013,2014,2020,periodic)

sip='127.0.0.1'
hip='127.0.0.1'
keycode='5J4C-26Q3-2262-22AT-B228'
version='4.B.S.D'

queryCount=0

print """copy gateway_log (gateway_key, txn_type_key, org_key, version,
source_ip, hp_ip, keycodestring, txn_time, created_date) from stdin;"""

s=Stats.Stats(3100)
s.start()

for boxnum in range(1000000,2000000):
    t = periodic
    # for t in onceTypes:
    for day in range(1,31):
        queryCount = queryCount + 1
        if queryCount % 10000 == 0:
            # sys.stderr.write(`queryCount`  + "...\n")
            s.stop()
            s.click()
            sys.stderr.write(s.getStats() + " (" + `queryCount` + ")\n")
            s.start()
        datestamp="2003/07/%02d %02d:%02d:%02d" % (day, \
            r.randint(0,23), r.randint(0,59), r.randint(0,59))
        print "\t".join((str(boxnum), str(t), str(getOrgKey()), version, \
            sip, hip, keycode, str(getTxnTime()), datestamp))
print "\\.";
