#!/usr/bin/env jython

# Optional debug
# -Dorg.apache.commons.logging.simplelog.showdatetime=true \
# -Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.SimpleLog \
# -Dorg.apache.commons.logging.simplelog.log.httpclient.wire=trace \
# -Dorg.apache.commons.logging.simplelog.log.org.apache.commons.httpclient=debug \

import sys

import java
import net

# To register the protocol
from org.apache.commons.httpclient.protocol import Protocol

# Errors
from org.apache.commons.httpclient import HttpRecoverableException
from org.apache.commons.httpclient.HttpConnection import ConnectionTimeoutException
from javax.net.ssl import SSLException

# SSL stuff
from com.twowire.app.mdcscrape import MDCSSLSocketFactory

# Handlers and stuff
from com.twowire.app.mdcscrape import MDCPageHandlerRegistry
from com.twowire.app.mdcscrape import MDCProcessor
from com.twowire.app.mdcscrape import SerializationQueue

from com.twowire.app.mdcscrape import MDCSystemSummaryHandler
from com.twowire.app.mdcscrape import MDCLinkSummaryHandler
from com.twowire.app.mdcscrape import MDCLinkStatsHandler
from com.twowire.app.mdcscrape import MDCLinkDetailedStatsHandler
from com.twowire.app.mdcscrape import MDCDeviceListHandler

class Recorder(net.spy.util.ThreadPoolObserver):

	def __init__(self):
		net.spy.util.ThreadPoolObserver.__init__(self)
		self.stats=net.spy.util.ProgressStats(100)
		self.stats.start()

		self.serQueues={}
		for i in range(10):
			os=java.io.FileOutputStream("results_" + `i` + ".srz")
			gos=java.util.zip.GZIPOutputStream(os)
			oos=java.io.ObjectOutputStream(gos);
			self.serQueues[i]=SerializationQueue("Serializer#" + `i`, oos)
			# Slightly higher than normal priority for these
			self.serQueues[i].setPriority(java.lang.Thread.NORM_PRIORITY + 1)
			self.serQueues[i].start()

		self.errors=java.io.PrintWriter(java.io.FileWriter("error.log"))

	def jobComplete(self, j):
		if j.wasSuccessful():
			sn=j.getSerialNumber()
			self.errors.println("Successful:  " + j.getSerialNumber());
			snf=int(sn[0])
			q=self.serQueues[snf]
			o=java.util.ArrayList()
			o.add(sn)
			o.add(j.getResults())
			q.addObjects(o)
		else:
			self.errors.print("Failed:  " + j.getSerialNumber() + "  ")
			# Figure out what to do with the exception
			e=j.getException()
			if isinstance(e, ConnectionTimeoutException):
				self.errors.println("timed out")
			elif isinstance(e, java.io.IOException):
				self.errors.println(e.getClass().getName()
					+ ": " + e.getMessage())
			else:
				self.errors.println("Exception:")
				j.getException().printStackTrace(self.errors)
		self.errors.flush()
		self.stats.stop()
		print "#", self.stats
		self.stats.start()

	def finished(self):
		for v in self.serQueues.values():
			v.close()
		self.errors.close()

# Start registering handlers and stuff

mph=MDCPageHandlerRegistry.getInstance()
# mph.registerHandler(mph.DEFAULT_HANDLER, PageHandler())

mph.registerHandler('system_summary.html', MDCSystemSummaryHandler())
mph.registerHandler('link_summary.html', MDCLinkSummaryHandler())
mph.registerHandler('link_statistics.html', MDCLinkStatsHandler())
mph.registerHandler('link_detailed_statistics.html',
	MDCLinkDetailedStatsHandler())
mph.registerHandler('network_device_list.html', MDCDeviceListHandler())

# Main
# get = GetMethod("https://462211000079:syhtob1ZmavwmayI8Y6GRg%3D%3D@208.35.230.144:50001/management/")

Protocol.registerProtocol("https",
	Protocol("https", MDCSSLSocketFactory(), 443))

# The pool monitor (that saves the stuff)
recorder=Recorder()

tp=net.spy.util.ThreadPool("Fetcher", 100)
tp.setStartThreads(100)
tp.setMinIdleThreads(5)
tp.setMonitor(recorder)
tp.start()

pageList=java.util.ArrayList(2)
pageList.add("network_device_list.html")
pageList.add("link_summary.html")
pageList.add("link_detailed_statistics.html")
pageList.add("system_summary.html")

l=sys.stdin.readline()
while l != '':
	(sn, vn, auth, url)=l.strip().split(" ")

	# print "# ", sn, vn, auth, url
	tp.addTask(MDCProcessor(sn, vn, auth, url, pageList))
	tp.waitForTaskCount(200)

	l=sys.stdin.readline()
tp.waitForCompletion()

# Finish the pool
recorder.finished()
