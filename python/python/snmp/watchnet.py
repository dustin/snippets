#!/usr/bin/env python
#
# $Id: watchnet.py,v 1.4 2002/04/04 20:00:47 dustin Exp $

import collector, jobs

if __name__ == '__main__':
	nc=collector.NetworkCollector()
	nc.initXMLRPC(9999)
	try:
		# Watch the interfaces on sw1 as volatile objects reporting when the
		# speed changes.  Check again every hour
		for i in range(1,26):
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 'ifSpeed.' + `i`, 60))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 'ifLastChange.' + `i`, 60))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 'ifInDiscards.' + `i`, 60))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 'ifInErrors.' + `i`, 60))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 'ifOutErrors.' + `i`, 60))
		# Juan's default route
		nc.addJob(jobs.VolatileSNMPJob(
			'juan', 'public', 'ipRouteNextHop.0.0.0.0', 300))
		# OK, also watch usage on juan's ethernets
		nc.addJob(jobs.RRDSNMPJob('juan', 'public',
			('ifInOctets.1', 'ifOutOctets.1'), 60, 'rrd/juan.ex0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('juan', 'public',
			('ifInOctets.2', 'ifOutOctets.2'), 60, 'rrd/juan.de0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('juan', 'public',
			('ifInOctets.3', 'ifOutOctets.3'), 60, 'rrd/juan.ex1.rrd'))
		# And dante's ethernets
		nc.addJob(jobs.RRDSNMPJob('dante', 'public',
			('ifInOctets.1', 'ifOutOctets.1'), 60, 'rrd/dante.sn0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('dante', 'public',
			('ifInOctets.2', 'ifOutOctets.2'), 60, 'rrd/dante.ae0.rrd'))
		nc.run()
	finally:
		print "Requesting stop."
		nc.stop()
