#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: report.py,v 1.1 2003/09/26 01:13:25 dustin Exp $
"""

import sys
import java

types={}
types['Ethernet']='eth'
types['USB']='usb'
types['USB Device']='usb'
types['HPNA 1.0']='hpna'
types['HPNA 2.0']='hpna'
types['Phoneline (HomePNA)']='hpna'
types['802.11b']='wireless'
types['Wireless (802.11b)']='wireless'
types['Unknown']='unknown'

def report(sn, data):
	vals=[]
	vals.append(sn)
	vals.append(data['system_summary.html']['uptime'])
	vals.append(data['link_summary.html']['accessConcentrator'])
	vals.append(data['link_detailed_statistics.html']['DSL Unavailable Seconds:Reset'])

	counts={}
	for i in types.values():
		counts[i]=0
	total=0
	for i in data['network_device_list.html']:
		if i[1] != '--':
			try:
				total = total + 1
				k=types[i[1]]
				counts[k] = counts[k] + 1
			except KeyError, e:
				sys.stderr.write("Broken record for " + sn + ":  " + `i` + "\n")

	vals.append(total)
	for c in ('eth', 'usb', 'hpna', 'wireless', 'unknown'):
		vals.append(counts[c])

	print "\t".join(map(str, vals))

print "#sn\tuptime\taccessConcentrator\tunavail\tdevices\teth\tusb\thpna\t" \
	+ "wireless\tunknown"

fis=java.io.FileInputStream(sys.argv[1])
gis=java.util.zip.GZIPInputStream(fis)
ois=java.io.ObjectInputStream(gis)

try:
	sn=ois.readObject()
	while sn != None:
		data=ois.readObject()

		report(sn, data)

		sn=ois.readObject()
except java.io.EOFException:
	sys.stderr.write("Finished!\n")
