#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: getmacs.py,v 1.1 2003/09/26 01:13:22 dustin Exp $
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
types['802.11b']='wifi'
types['Wireless (802.11b)']='wifi'
types['Unknown']='unknown'

def report(sn, data):

	macs=[]
	for i in data['network_device_list.html']:
		if i[1] != '--':
			try:
				k=types[i[1]]
				macs.append(i[2] + "\t" + k)
			except KeyError, e:
				sys.stderr.write("Broken record for " + sn + ":  " + `i` + "\n")

	print sn
	for m in macs:
		print "\t" + m


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
