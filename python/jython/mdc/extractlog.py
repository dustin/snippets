#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: dump.py,v 1.1 2003/09/26 01:13:22 dustin Exp $
"""

import sys
import java

def report(sn, data):
	for l in data['debuglog.html']:
		print sn, l

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
