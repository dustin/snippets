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

# This holds the names of the columns we're going to produce
colnames=['sn', 'devices', 'eth', 'usb', 'hpna', 'wireless', 'unknown']
# This will hold a bunch of tuples of column names to column path mapping,
# where the column path is itself a tuple of map names

# The following is generated via autoheader.py
extracols=[
	('ATMrecv', ('link_statistics.html', 'ATMrecv')),
	('ATMxmit', ('link_statistics.html', 'ATMxmit')),
	('Cell Header Errors:15m', ('link_detailed_statistics.html', 'Cell Header Errors:15m')),
	('Cell Header Errors:24Hr', ('link_detailed_statistics.html', 'Cell Header Errors:24Hr')),
	('Cell Header Errors:Reset', ('link_detailed_statistics.html', 'Cell Header Errors:Reset')),
	('Cell Header Errors:Since', ('link_detailed_statistics.html', 'Cell Header Errors:Since')),
	('Corrected Blocks:15m', ('link_detailed_statistics.html', 'Corrected Blocks:15m')),
	('Corrected Blocks:24Hr', ('link_detailed_statistics.html', 'Corrected Blocks:24Hr')),
	('Corrected Blocks:Reset', ('link_detailed_statistics.html', 'Corrected Blocks:Reset')),
	('Corrected Blocks:Since', ('link_detailed_statistics.html', 'Corrected Blocks:Since')),
	('Cumulative Sec. w/Severe Errors:15m', ('link_detailed_statistics.html', 'Cumulative Sec. w/Severe Errors:15m')),
	('Cumulative Sec. w/Severe Errors:24Hr', ('link_detailed_statistics.html', 'Cumulative Sec. w/Severe Errors:24Hr')),
	('Cumulative Sec. w/Severe Errors:Reset', ('link_detailed_statistics.html', 'Cumulative Sec. w/Severe Errors:Reset')),
	('Cumulative Sec. w/Severe Errors:Since', ('link_detailed_statistics.html', 'Cumulative Sec. w/Severe Errors:Since')),
	('Cumulative Seconds w/Errors:15m', ('link_detailed_statistics.html', 'Cumulative Seconds w/Errors:15m')),
	('Cumulative Seconds w/Errors:24Hr', ('link_detailed_statistics.html', 'Cumulative Seconds w/Errors:24Hr')),
	('Cumulative Seconds w/Errors:Reset', ('link_detailed_statistics.html', 'Cumulative Seconds w/Errors:Reset')),
	('Cumulative Seconds w/Errors:Since', ('link_detailed_statistics.html', 'Cumulative Seconds w/Errors:Since')),
	('DSL Training Errors:15m', ('link_detailed_statistics.html', 'DSL Training Errors:15m')),
	('DSL Training Errors:24Hr', ('link_detailed_statistics.html', 'DSL Training Errors:24Hr')),
	('DSL Training Errors:Reset', ('link_detailed_statistics.html', 'DSL Training Errors:Reset')),
	('DSL Training Errors:Since', ('link_detailed_statistics.html', 'DSL Training Errors:Since')),
	('DSL Unavailable Seconds:15m', ('link_detailed_statistics.html', 'DSL Unavailable Seconds:15m')),
	('DSL Unavailable Seconds:24Hr', ('link_detailed_statistics.html', 'DSL Unavailable Seconds:24Hr')),
	('DSL Unavailable Seconds:Reset', ('link_detailed_statistics.html', 'DSL Unavailable Seconds:Reset')),
	('DSL Unavailable Seconds:Since', ('link_detailed_statistics.html', 'DSL Unavailable Seconds:Since')),
	('IPrecv', ('link_statistics.html', 'IPrecv')),
	('IPxmit', ('link_statistics.html', 'IPxmit')),
	('ISP Connection Establishment:15m', ('link_detailed_statistics.html', 'ISP Connection Establishment:15m')),
	('ISP Connection Establishment:24Hr', ('link_detailed_statistics.html', 'ISP Connection Establishment:24Hr')),
	('ISP Connection Establishment:Reset', ('link_detailed_statistics.html', 'ISP Connection Establishment:Reset')),
	('ISP Connection Establishment:Since', ('link_detailed_statistics.html', 'ISP Connection Establishment:Since')),
	('Link Retrains:15m', ('link_detailed_statistics.html', 'Link Retrains:15m')),
	('Link Retrains:24Hr', ('link_detailed_statistics.html', 'Link Retrains:24Hr')),
	('Link Retrains:Reset', ('link_detailed_statistics.html', 'Link Retrains:Reset')),
	('Link Retrains:Since', ('link_detailed_statistics.html', 'Link Retrains:Since')),
	('Loss of Cell Delineation:15m', ('link_detailed_statistics.html', 'Loss of Cell Delineation:15m')),
	('Loss of Cell Delineation:24Hr', ('link_detailed_statistics.html', 'Loss of Cell Delineation:24Hr')),
	('Loss of Cell Delineation:Reset', ('link_detailed_statistics.html', 'Loss of Cell Delineation:Reset')),
	('Loss of Cell Delineation:Since', ('link_detailed_statistics.html', 'Loss of Cell Delineation:Since')),
	('Loss of Framing Failures:15m', ('link_detailed_statistics.html', 'Loss of Framing Failures:15m')),
	('Loss of Framing Failures:24Hr', ('link_detailed_statistics.html', 'Loss of Framing Failures:24Hr')),
	('Loss of Framing Failures:Reset', ('link_detailed_statistics.html', 'Loss of Framing Failures:Reset')),
	('Loss of Framing Failures:Since', ('link_detailed_statistics.html', 'Loss of Framing Failures:Since')),
	('Loss of Margin Failures:15m', ('link_detailed_statistics.html', 'Loss of Margin Failures:15m')),
	('Loss of Margin Failures:24Hr', ('link_detailed_statistics.html', 'Loss of Margin Failures:24Hr')),
	('Loss of Margin Failures:Reset', ('link_detailed_statistics.html', 'Loss of Margin Failures:Reset')),
	('Loss of Margin Failures:Since', ('link_detailed_statistics.html', 'Loss of Margin Failures:Since')),
	('Loss of Power Failures:15m', ('link_detailed_statistics.html', 'Loss of Power Failures:15m')),
	('Loss of Power Failures:24Hr', ('link_detailed_statistics.html', 'Loss of Power Failures:24Hr')),
	('Loss of Power Failures:Reset', ('link_detailed_statistics.html', 'Loss of Power Failures:Reset')),
	('Loss of Power Failures:Since', ('link_detailed_statistics.html', 'Loss of Power Failures:Since')),
	('Loss of Signal Failures:15m', ('link_detailed_statistics.html', 'Loss of Signal Failures:15m')),
	('Loss of Signal Failures:24Hr', ('link_detailed_statistics.html', 'Loss of Signal Failures:24Hr')),
	('Loss of Signal Failures:Reset', ('link_detailed_statistics.html', 'Loss of Signal Failures:Reset')),
	('Loss of Signal Failures:Since', ('link_detailed_statistics.html', 'Loss of Signal Failures:Since')),
	('Training Timeouts:15m', ('link_detailed_statistics.html', 'Training Timeouts:15m')),
	('Training Timeouts:24Hr', ('link_detailed_statistics.html', 'Training Timeouts:24Hr')),
	('Training Timeouts:Reset', ('link_detailed_statistics.html', 'Training Timeouts:Reset')),
	('Training Timeouts:Since', ('link_detailed_statistics.html', 'Training Timeouts:Since')),
	('Uncorrectable Blocks:15m', ('link_detailed_statistics.html', 'Uncorrectable Blocks:15m')),
	('Uncorrectable Blocks:24Hr', ('link_detailed_statistics.html', 'Uncorrectable Blocks:24Hr')),
	('Uncorrectable Blocks:Reset', ('link_detailed_statistics.html', 'Uncorrectable Blocks:Reset')),
	('Uncorrectable Blocks:Since', ('link_detailed_statistics.html', 'Uncorrectable Blocks:Since')),
	('accessConcentrator', ('link_summary.html', 'accessConcentrator')),
	('attDown', ('link_statistics.html', 'attDown')),
	('attUp', ('link_statistics.html', 'attUp')),
	('channel', ('link_summary.html', 'channel')),
	('currentDown', ('link_statistics.html', 'currentDown')),
	('currentUp', ('link_statistics.html', 'currentUp')),
	('dslam', ('link_summary.html', 'dslam')),
	('maxDown', ('link_statistics.html', 'maxDown')),
	('maxUp', ('link_statistics.html', 'maxUp')),
	('noiseDown', ('link_statistics.html', 'noiseDown')),
	('noiseUp', ('link_statistics.html', 'noiseUp')),
	('powerDown', ('link_statistics.html', 'powerDown')),
	('powerUp', ('link_statistics.html', 'powerUp')),
	('protocol', ('link_summary.html', 'protocol')),
	('systime', ('system_summary.html', 'systime')),
	('uptime', ('system_summary.html', 'uptime')),
	('dslLine', ('link_summary.html', 'dslLine')),
	('username', ('link_summary.html', 'username'))
]

for f in sys.argv[1:]:
	fis=java.io.FileInputStream(f)
	gis=java.util.zip.GZIPInputStream(fis)
	ois=java.io.ObjectInputStream(gis)

	# Print the report column headers
	print "#" + "\t".join(colnames) + "\t" \
		+ "\t".join(map(lambda x: x[0], extracols))

	try:
		sn=ois.readObject()
		while sn != None:
			data=ois.readObject()

			report(sn, data, extracols)

			sn=ois.readObject()
	except java.io.EOFException:
		sys.stderr.write("Finished " + f + "!\n")
