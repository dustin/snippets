#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: report.py,v 1.1 2003/09/26 01:13:25 dustin Exp $
"""

import sys
import java

def report(sn, data):
	x = data["dsl_dsm_diags.txt"]
	if x is not None:
		for k in x.keySet():
			for v in x[k]:
				print sn, k, v

for f in sys.argv[1:]:
	fis=java.io.FileInputStream(f)
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

	ois.close()
	gis.close()
	fis.close()
