#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

# Generate input files for mdc scraping from a phone number to serial number
# list.

# # Input:
# 408-528-8699,423116024600

# # Desired output:
# 0,0,443118043057,443118043057.mdc.cms.2wire.com

import os
import sys
import glob
import string

def usage():
    sys.stderr.write("Usage:  %s inputDir mdcInput mdcPhoneMap\n"
        % (sys.argv[0]))
    sys.exit(1)

try:
    thedir=sys.argv[1]
    infileName=sys.argv[2]
    phoneMapName=sys.argv[3]
except IndexError:
    usage()

theInput=open(infileName, 'w')
thePhoneMap=open(phoneMapName, 'w')

for thefile in glob.glob1(thedir, "*.csv"):
    fn=os.path.join(thedir, thefile)
    batchname=thefile[0:-4]

    f=open(fn)
    for l in f.readlines():
        try:
            l=string.strip(l)
            pn,sn = string.split(l, ",")
            pn=string.strip(pn)
            sn=string.strip(sn)
            thePhoneMap.write(pn + "," + sn + "\n")
            theInput.write(batchname + ",0," + sn + ","
                + sn + ".mdc.cms.2wire.com" + "\n")
        except ValueError:
            sys.stderr.write("``" + l + "'' is broken\n")
    f.close()

theInput.close()
thePhoneMap.close()
