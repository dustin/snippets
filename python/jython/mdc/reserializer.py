#!/usr/bin/env jython
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: srzreader.py,v 1.1 2003/09/09 20:20:27 dustin Exp $
"""

import sys
import string
import java
import com
import net

import dbrecorder
# import filecopy

class MyProcessor(com.twowire.app.mdcscrape.MDCProcessor):
	def __init__(self, sn, data):
		com.twowire.app.mdcscrape.MDCProcessor.__init__(self, sn, data)

def getWriter(fn):
	fos=java.io.FileOutputStream(fn)
	gos=java.util.zip.GZIPOutputStream(fos)
	osw=java.io.OutputStreamWriter(gos)
	return osw

# the destination
# recorder=com.twowire.app.mdcscrape.SerializingRecorder("test.log",
	# sys.argv[2], 0)

observer=com.twowire.app.mdcscrape.MDCObserver()
conf=net.spy.SpyConfig(sys.argv[1])
# recorder=filecopy.DBFileRecorder(conf)
recorder=com.twowire.app.mdcscrape.DelimitedFileRecorder(conf,
	getWriter("txfile.txt.gz"),
	getWriter("dfile.txt.gz"),
	getWriter("devices.txt.gz"))

observer.registerRecorder(recorder)

def getOrgFromPath(f):
	a=string.split(f, "/")
	return java.lang.Integer(a[-2])

# The source
processed=0
for f in sys.argv[2:]:
	print "Starting " + f
	# org=getOrgFromPath(f)
	fis=java.io.FileInputStream(f)
	gis=java.util.zip.GZIPInputStream(fis)
	ois=java.io.ObjectInputStream(gis)

	try:
		sn=ois.readObject()
		while sn != None:
			data=ois.readObject()
			if data.get("orgKey") is None:
				print "Applying org id:  " + `org`
				data.put("orgKey", org)

			# Send it through the serializer
			observer.jobComplete(MyProcessor(sn, data))

			processed=processed+1

			# Get the next serial number
			sn=ois.readObject()

	except java.io.IOException, e:
		if isinstance(e, java.io.EOFException):
			print "Finished " + f
		else:
			e.printStackTrace()

	fis.close()
	gis.close()
	ois.close()

recorder.finished()
print "Finished with " + `processed` + " transactions"
