#!/usr/bin/env python
#
# $Id: watchnet.py,v 1.6 2002/05/01 20:33:46 dustin Exp $

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
		nc.addJob(jobs.RRDSNMPJob('disk', 'public',
			('ifInOctets.1', 'ifOutOctets.1'), 60, 'rrd/disk_hme0.rrd'))
		nc.addJob(jobs.RRDSNMPJob('binge.b', 'public',
			('ifInOctets.1', 'ifOutOctets.1'), 60, 'rrd/binge_le0.rrd'))
		# ip, udp, tcp:
		for host in ('dante', 'juan', 'disk', 'binge.b'):
			nc.addJob(jobs.RRDSNMPJob(host, 'public',
				('ipInReceives.0', 'ipInDelivers.0'), 60,
				'rrd/' + host + '_ip_in.rrd'))
			nc.addJob(jobs.RRDSNMPJob(host, 'public',
				('udpInDatagrams.0', 'udpOutDatagrams.0'), 60,
				'rrd/' + host + '_udp_in.rrd'))
			nc.addJob(jobs.RRDSNMPJob(host, 'public',
				('tcpInSegs.0', 'tcpOutSegs.0'), 60,
				'rrd/' + host + '_tcp_in.rrd'))
			nc.addJob(jobs.VolatileSNMPJob(
				host, 'public', 'ifInDiscards.1', 60))
			nc.addJob(jobs.VolatileSNMPJob(
				host, 'public', 'ifInErrors.1', 60))
			nc.addJob(jobs.VolatileSNMPJob(
				host, 'public', 'ifOutErrors.1', 60))
		nc.run()
	finally:
		print "Requesting stop."
		nc.stop()
