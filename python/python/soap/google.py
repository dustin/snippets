#!/usr/bin/env python
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# $Id: google.py,v 1.1 2002/04/15 23:35:11 dustin Exp $

import sys
import SOAP

myKey='2hOO7zk9TTDrPe0fpnxR0Yv/5K66pVHX'
# SOAP.Config.debug=1
server = SOAP.SOAPProxy("http://api.google.com/search/beta2",
	namespace='urn:GoogleSearch')

soapfalse=SOAP.booleanType(0)
soaptrue=SOAP.booleanType(1)

s=SOAP.stringType

query=sys.argv[1]

keepGoing=1
start=0
lastStart=0
nResults=10
while keepGoing:

	# key, search string, start, results, filter, restrict, safe, lr, ie, oe
	results=server.doGoogleSearch(s(myKey), s(query),
		start, nResults, soaptrue, s(''), soapfalse, s(''), s(''), s(''))

	extra=''
	if results['estimateIsExact']:
		extra=' (exact)'
	print "Estimated results for " + `query` + ":  " \
		+ `results['estimatedTotalResultsCount']` + extra
	if len(results['searchTips'])>0:
		print "Tips:  " + results['searchTips']
	if len(results['searchComments']) > 0:
		print "Comments:  " + results['searchComments']
	print "Results " + `results['startIndex']` + '-' + `results['endIndex']` \
		+ ':'
	for r in results['resultElements']:
		try:
			print "\t" + r['URL'] + " - " + r['title']
		except UnicodeError:
			print "\t" + r['URL'] + ' - <UNICODE ERROR>'

	# Figure out whether we should continue
	start=results['endIndex']
	if start == lastStart or (not start == (lastStart+nResults)):
		keepGoing=None
	lastStart=start
