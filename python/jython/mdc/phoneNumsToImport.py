#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

# Generate input files for mdc scraping from a phone number to serial number
# list.

# # Input:
# 408-528-8699,423116024600

# # Desired output:
# 0,0,443118043057,3.5.9

import os
import sys
import glob

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

for batchname in glob.glob1(thedir, "*"):
	fn=os.path.join(thedir, batchname)

	f=open(fn)
	for l in f.readlines():
		l=l.strip()
		pn,sn = l.split(",")
		thePhoneMap.write(pn + "," + sn + "\n")
		theInput.write(batchname + ",0," + sn + ",3.5.9" + "\n")
	f.close()

theInput.close()
thePhoneMap.close()
