#!/usr/bin/env python2.2
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: statwatch.py,v 1.4 2003/07/17 16:37:44 dustin Exp $
"""

import sys
import time
import string
import urllib
import traceback

def updateHost(url, h, prefix=""):
	"""Update the dict h with the values from the given URL (matching the
		prefix"""
	u=urllib.URLopener()
	r=u.open(url)
	for l in r.readlines():
		l=l.rstrip()
		(k, v) = l.split(' = ')
		if(k.startswith(prefix)):
			v=int(v)
			ov=h.get(k, 0)
			h[k] = (v+ov)

def signed(x):
	rv=""
	if rv < 0:
		rv = "-" + `x`
	else:
		rv = "+" + `x`
	return rv

def updateAll(urls, h, prefix=""):
	"""Update all of the URLs into the given dict.  Return the deltas"""
	deltas={}
	tmp={}
	for u in urls:
		try:
			updateHost(u, tmp, prefix)
		except IOError:
			print "***  Error on " + str(u) + ":"
			print "***   " + traceback.format_exception_only(\
				sys.exc_type, sys.exc_value)[0]

	# Now that we've got all the individuals, add them all up
	ks=tmp.keys()
	ks.sort()
	for k in ks:
		v=tmp[k]
		ov=h.get(k, 0)
		if v != ov:
			d=(v-ov)
			deltas[k]=d
			# Display the current value
			print k + ":  " + `v` + " (" + signed(d) + ")"
		h[k]=v
	return deltas

def showTimes(times, readings):
	for timeset in times:
		try:
			label=timeset[0]
			timing=float(readings[timeset[1]])
			transcount=0.0
			for k in readings.keys():
				if k.startswith(timeset[2]):
					transcount = transcount + float(readings[k])
			if transcount > 0 and timing > 0:
				rate=((timing / transcount)/1000)
				print "%s: %.2fs/t" % (label, rate)
		except KeyError:
			# No data found for this key
			pass

if __name__ == '__main__':

	# Build the list of servers
	servers={}
	# Noc0
	tmp=[]
	for n in range(1,9):
		s="http://w51-" + str(n) + ".diag.c51.2wire.com:8080" \
			+ '/admin/monitor/stat'
		tmp.append(s)
	servers['noc0'] = tmp
	servers['production'] = tmp
	# Staging
	tmp=[]
	for n in range(1,5):
		s="http://w50-" + str(n) + ".diag.c50.2wire.com:8080" \
			+ '/admin/monitor/stat'
		tmp.append(s)
	servers['noc1'] = tmp
	servers['staging'] = tmp
	# Farooq
	servers['noc5']=['http://noc.noc5.2wire.com/admin/monitor/stat']
	# Dustin
	servers['noc13']=\
		['http://desktop.dsallings.eng.2wire.com/admin/monitor/stat']

	# Time calculation stuff
	timeCalcs=( ('Heartbeat', 'rpc.time.HB', 'rpc.success.CMS_HEARTBEAT'),
				('Bootstrap' ,'rpc.time.BOOT', 'rpc.success.CMS_BOOTSTRAP'),
				('Kick', 'rpc.time.KICK' ,'rpc.success.CMS_KICKED'),
			)

	h={}
	hparam=''
	if len(sys.argv) > 1:
		hparam=sys.argv[1]

	# Get the server list
	u=servers.get(hparam, None)

	# If we didn't get one, print a nice happy error
	if u is None:
		snames=servers.keys()
		snames.sort()
		print ""
		print "The following clusters are available:"
		for s in snames:
			print "\t" + s
		print ""
		raise "Invalid cluster:  " + sys.argv[1]
	print "Using the following URLs:  " + `u`

	# Check to see if a prefix was applied
	prefix=""
	if len(sys.argv) > 2:
		prefix=sys.argv[2]
		print "Limited to this prefix:  " + prefix

	# Loop forever
	while 1:
		print "--------------------------------------- " \
			+ time.ctime(time.time())
		deltas=updateAll(u, h, prefix)
		print ""
		showTimes(timeCalcs, deltas)
		print ""
		time.sleep(60)
