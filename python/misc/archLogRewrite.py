#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: skeleton.py,v 1.2 2003/01/03 08:43:26 dustin Exp $
"""

import time
import sys
import os
import string
import traceback

def rewriteFile(filename):

	f=open(filename)
	lines=f.readlines()
	f.close()

	# find the date
	ttable=string.maketrans("/:", "  ")
	lines.reverse()
	headers={}
	for l in lines:
		if l.startswith("Date: "):
			try:
				# Date: 1998/10/19 03:32:40
				d=l[6:]
				dp=map(int, string.split(string.translate(d, ttable)))
				headers["Standard-date"]="%04d-%02d-%02d %02d:%02d:%02d GMT" % \
					tuple(dp)
				os.environ["TZ"]="UTC"
				time.tzset()
				t=time.mktime((dp[0], dp[1], dp[2], dp[3],
					dp[4], dp[5], 0, 0, -1))
				del os.environ["TZ"]
				time.tzset()
				# headers["Date"]=time.ctime(t)
				# Fri Oct 17 00:51:34 PDT 2003
				headers["Date"]=time.strftime("%a %b %d %H:%M:%S %Z %Y",
					time.localtime(t))
			except ValueError:
				pass
			except:
				traceback.print_exc()

	sys.stderr.write(repr(headers) + "\n")

	if (len(headers) > 0):
		out=open(filename + ".tmp", "w")

		lines.reverse()
		allheaders=0
		for l in lines:
			if allheaders == 0:
				h=string.split(l, ":", 2)
				if headers.has_key(h[0]):
					out.write("%s: %s\n" % (h[0], headers[h[0]]))
				else:
					out.write(l)
				if l == "\n":
					allheaders=1
			else:
				out.write(l)

		os.rename(filename + ".tmp", filename)
	else:
		sys.stderr.write("No changes, not modifying log.\n")

# MAIN
for f in sys.argv[1:]:
	rewriteFile(f)
