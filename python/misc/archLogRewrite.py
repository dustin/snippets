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

modified = 0

def sanitize(input):
	rv=input
	if len(input)>60:
		rv=input[:55]
		rv=rv+"..."
	return(rv)

def extractSummary(lines):
	rv=''
	lines.reverse()
	section=1
	for l in lines:
		if section == 1:
			if l == '\n':
				section = 2
			elif l.startswith("Summary:"):
				origSummary=l[9:].strip()
		else:
			if l.startswith("Author:") or l.startswith("Date:"):
				pass
			else:
				if rv == '':
					test=l.strip()
					if test != '':
						rv=sanitize(test)
	lines.reverse()
	if rv == '':
		print "Can't find a good summary from the lines."
		rv=origSummary
	return(rv)

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
		elif l.startswith("Summary:"):
			summary=l[9:].strip()
			print "Summary is " + summary
			if summary == "..." or (summary.find("cscvs to tla") > -1):
				summary=extractSummary(lines)
				print "Forcing change to " + summary
				headers["Summary"]=summary

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
		global modified
		modified = modified + 1
	else:
		sys.stderr.write("No changes, not modifying log.\n")

# MAIN
for f in sys.argv[1:]:
	rewriteFile(f)

# Exit 0 if we modified stuff, else exit 1 (unless there was an error)
ev=0
if modified == 0:
	ev=1
sys.exit(ev)
