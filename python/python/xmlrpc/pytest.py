#!/usr/bin/env python
#
# $Id: pytest.py,v 1.2 2002/03/26 11:10:05 dustin Exp $

from sys import argv
import xmlrpclib
import time

server=xmlrpclib.Server('http://localhost:8000/RPC2')

totaltime=0
runs=100

method = getattr(server, argv[1])
for i in range(runs):
	starttime=time.time()
	print method('other arg call #' + str(i))
	stoptime=time.time()
	print "RTT:  " + str(stoptime-starttime)
	totaltime = totaltime + (stoptime-starttime)

print "Average RTT:  " + str(totaltime/runs)
