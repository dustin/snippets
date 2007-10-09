#!/usr/bin/env python
#
# $Id: watchnet.py,v 1.7 2002/05/07 20:55:37 dustin Exp $

import collector, jobs

if __name__ == '__main__':
	nc=collector.NetworkCollector()
	nc.initXMLRPC(9999)
	try:
		# Watch the interfaces on sw1 as volatile objects reporting when the
		# speed changes.  Check again every hour
		for i in range(1,26):
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 60, 'ifSpeed.' + `i`))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 60, 'ifLastChange.' + `i`))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 60, 'ifInDiscards.' + `i`))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 60, 'ifInErrors.' + `i`))
			nc.addJob(jobs.VolatileSNMPJob(
				'sw1', 'public', 60, 'ifOutErrors.' + `i`))
		# Juan's default route
		nc.addJob(jobs.VolatileSNMPJob(
			'juan', 'public', 300, 'ipRouteNextHop.0.0.0.0'))
		# OK, also watch usage on juan's ethernets
		nc.addJob(jobs.RRDSNMPJob('juan', 'public',
			60, ('ifInOctets.1', 'ifOutOctets.1'), 'rrd/juan.ex0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('juan', 'public',
			60, ('ifInOctets.2', 'ifOutOctets.2'), 'rrd/juan.de0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('juan', 'public',
			60, ('ifInOctets.3', 'ifOutOctets.3'), 'rrd/juan.ex1.rrd'))
		# And dante's ethernets
		nc.addJob(jobs.RRDSNMPJob('dante', 'public',
			60, ('ifInOctets.1', 'ifOutOctets.1'), 'rrd/dante.sn0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('dante', 'public',
			60, ('ifInOctets.2', 'ifOutOctets.2'), 'rrd/dante.ae0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('disk', 'public',
			60, ('ifInOctets.1', 'ifOutOctets.1'), 'rrd/disk_hme0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('binge.b', 'public',
			60, ('ifInOctets.1', 'ifOutOctets.1'), 'rrd/binge_le0.rrd'))
		# ip, udp, tcp:
		for host in ('dante', 'juan', 'disk', 'binge.b'):
			nc.addJob(jobs.RRDSNMPJob(host, 'public',
				60, ('ipInReceives.0', 'ipInDelivers.0'),
				'rrd/' + host + '_ip_in.rrd'))
			nc.addJob(jobs.RRDSNMPJob(host, 'public',
				60, ('udpInDatagrams.0', 'udpOutDatagrams.0'),
				'rrd/' + host + '_udp_in.rrd'))
			nc.addJob(jobs.RRDSNMPJob(host, 'public',
				60, ('tcpInSegs.0', 'tcpOutSegs.0'),
				'rrd/' + host + '_tcp_in.rrd'))
			nc.addJob(jobs.VolatileSNMPJob(
				host, 'public', 60, 'ifInDiscards.1'))
			nc.addJob(jobs.VolatileSNMPJob(
				host, 'public', 60, 'ifInErrors.1'))
			nc.addJob(jobs.VolatileSNMPJob(
				host, 'public', 60, 'ifOutErrors.1'))
		nc.run()
	finally:
		print "Requesting stop."
		nc.stop()
