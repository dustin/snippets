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

# SSL stuff
from com.twowire.app.mdcscrape import MDCSSLSocketFactory

# Handlers and stuff
from com.twowire.app.mdcscrape import MDCPageHandlerRegistry
from com.twowire.app.mdcscrape import MDCProcessor
from com.twowire.app.mdcscrape import MDCObserver
from com.twowire.app.mdcscrape import SimpleLoggingRecorder
from com.twowire.app.mdcscrape import SerializingRecorder

from com.twowire.app.mdcscrape import MDCSystemSummaryHandler
from com.twowire.app.mdcscrape import MDCLinkSummaryHandler
from com.twowire.app.mdcscrape import MDCLinkStatsHandler
from com.twowire.app.mdcscrape import MDCLinkDetailedStatsHandler
from com.twowire.app.mdcscrape import MDCDeviceListHandler
from com.twowire.app.mdcscrape import MDCDebugLogHandler
from com.twowire.app.mdcscrape import AbstractMDCPageHandler

class DebugHandler(AbstractMDCPageHandler):

	def __init__(self):
		AbstractMDCPageHandler.__init__(self)
		self.prefixes={"DBG":1, "INF":1, "WRN":1, "ERR":1}

	def isLog(self, msg):
		rv=0
		if len(msg) < 4:
			rv=0
		elif self.prefixes.has_key(msg[0:3]):
			rv=1
		return rv

	def handle(self, l):
		rv=java.util.ArrayList()
		for line in l:
			if self.isLog(line):
				rv.add(line)
		return rv

# Start registering handlers and stuff

mph=MDCPageHandlerRegistry.getInstance()
# mph.registerHandler(mph.DEFAULT_HANDLER, PageHandler())

mph.registerHandler('system_summary.html', MDCSystemSummaryHandler())
mph.registerHandler('link_summary.html', MDCLinkSummaryHandler())
mph.registerHandler('link_statistics.html', MDCLinkStatsHandler())
mph.registerHandler('link_detailed_statistics.html',
	MDCLinkDetailedStatsHandler())
mph.registerHandler('network_device_list.html', MDCDeviceListHandler())
# mph.registerHandler('debuglog.html', DebugHandler())
mph.registerHandler('debuglog.html', MDCDebugLogHandler())

# Main
# get = GetMethod("https://462211000079:syhtob1ZmavwmayI8Y6GRg%3D%3D@208.35.230.144:50001/management/")

Protocol.registerProtocol("https",
	Protocol("https", MDCSSLSocketFactory(), 443))

# The pool monitor (that saves the stuff)
observer=MDCObserver()
observer.registerRecorder(SerializingRecorder("results.srz"))
observer.registerRecorder(SimpleLoggingRecorder("mdc.log", int(sys.argv[1])))

tp=net.spy.util.ThreadPool("Fetcher", 10)
tp.setStartThreads(10)
tp.setMinIdleThreads(5)
tp.setMonitor(observer)
tp.start()

pageList=java.util.ArrayList(2)
# pageList.add("link_detailed_statistics.html")
# pageList.add("link_statistics.html")
# pageList.add("link_summary.html")
# pageList.add("network_device_list.html")
# pageList.add("system_summary.html")
pageList.add("debuglog.html")

l=sys.stdin.readline()
while l != '':
	(sn, vn, auth, url)=l.strip().split(" ")

	# print "# ", sn, vn, auth, url
	tp.addTask(MDCProcessor(3, sn, vn, auth, url, pageList))
	tp.waitForTaskCount(200)

	l=sys.stdin.readline()
tp.waitForCompletion()

# Finish the pool
observer.finished()
