#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: autoreport.py,v 1.1 2003/09/26 01:13:21 dustin Exp $
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

def getColFromData(col, data):
	desc=col[1]
	rv=0
	try:
		rv=data[desc[0]][desc[1]]
	except KeyError, e:
		pass
	return rv

def report(sn, data, extracols):
	vals=[]
	vals.append(sn)

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

	for e in extracols:
		vals.append(getColFromData(e, data))

	print "\t".join(map(str, vals))

def calculateCols(data):
	rv=[]
	for k1 in data.keySet():
		v1=data[k1]
		if isinstance(v1, java.util.Map):
			for k2 in data[k1].keySet():
				v2=v1[k2]
				if not (isinstance(v2, java.util.Map)
						or isinstance(v2, java.util.Collection)):
					rv.append( (k2, (k1, k2) ))
	return(rv)


# This holds the names of the columns we're going to produce
colnames=['sn', 'devices', 'eth', 'usb', 'hpna', 'wireless', 'unknown']
# This will hold a bunch of tuples of column names to column path mapping,
# where the column path is itself a tuple of map names
extracols=[]

fis=java.io.FileInputStream(sys.argv[1])
gis=java.util.zip.GZIPInputStream(fis)
ois=java.io.ObjectInputStream(gis)

try:
	sn=ois.readObject()
	while sn != None:
		data=ois.readObject()

		# If we haven't initialized the extra cols, do so now.
		if len(extracols) == 0:
			extracols = calculateCols(data)
			print "#" + "\t".join(colnames) + "\t" \
				+ "\t".join(map(lambda x: x[0], extracols))

		report(sn, data, extracols)

		sn=ois.readObject()
except java.io.EOFException:
	sys.stderr.write("Finished!\n")
