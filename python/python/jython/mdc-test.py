#!/usr/bin/env jython

# Optional debug
# -Dorg.apache.commons.logging.simplelog.showdatetime=true \
# -Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.SimpleLog \
# -Dorg.apache.commons.logging.simplelog.log.httpclient.wire=trace \
# -Dorg.apache.commons.logging.simplelog.log.org.apache.commons.httpclient=debug \

import jarray
import time

from java.util import StringTokenizer

from net.spy import SpyUtil

from org.apache.commons.httpclient import HttpClient
from org.apache.commons.httpclient import UsernamePasswordCredentials
from org.apache.commons.httpclient.methods import GetMethod

from org.apache.commons.httpclient import UsernamePasswordCredentials

# SSL stuff
from com.sun.net.ssl import SSLContext
from com.sun.net.ssl import TrustManager
from org.apache.commons.httpclient.protocol import SecureProtocolSocketFactory

from com.sun.net.ssl import TrustManagerFactory
from com.sun.net.ssl import TrustManager
from com.sun.net.ssl import X509TrustManager
from org.apache.commons.httpclient.protocol import Protocol

class EasyX509TrustManager(X509TrustManager):

	def isClientTrusted(self, certs):
		return 1

	def isServerTrusted(self, certs):
		return 1

	def getAcceptedIssuers(self):
		return self.standardTrustManager.getAcceptedIssuers()

class MySSLSocketFactory(SecureProtocolSocketFactory):

	def createSocket(self, *args):
		context = SSLContext.getInstance("SSL")
		managers=jarray.array([EasyX509TrustManager()], X509TrustManager)
		context.init(None, managers, None)
		sf=context.getSocketFactory()

		rv=None
		if len(args) == 4:
			rv=sf.createSocket(args[0], args[1], args[2], args[3])
		elif len(args) == 2:
			rv=sf.createSocket(args[0], args[1])
		else:
			raise "Don't know about " + `len(args)` + " args."
		return rv

class PageHandler:
	"""Superclass for all handlers"""

	def handle(self, lines):
		print "NOOP HANDLER"

class SystemSummaryHandler(PageHandler):
	"""Handle the system summary page"""

	def handle(self, lines):
		print "SYSTEM SUMMARY HANDLER"
		stuff={}
		part="START"
		component=None
		for i in range(len(lines)):
			l=lines[i]
			if l == "Time Since Last Boot:":
				stuff['uptime']=lines[i+1]
			elif l == 'System Time:':
				stuff['systime']=lines[i+1] + " " + lines[i+2]
			elif l == "Components":
				part = l
				stuff['components']={}
			else:
				if part == "Components":
					if l.endswith(":"):
						component=l
					else:
						stuff['components'][component]=l

		print stuff

class LinkSummaryHandler(PageHandler):
	"""Handle the link summary page"""

	def handle(self, lines):
		print "LINK SUMMARY HANDLER"
		stuff={}
		for i in range(len(lines)):
			l = lines[i]
			if l == "Protocol:":
				stuff['protocol']=lines[i+1]
			elif l == "DSL Channel:":
				stuff['channel']=lines[i+1]
			elif l == "DSLAM:":
				stuff['dslam']=lines[i+1]
			elif l == "PPPoE Access Concentrator:":
				stuff['accessConcentrator']=lines[i+1]

		print stuff

class LinkStatsHandler(PageHandler):
	"""Handle the link stats page"""

	def handle(self, lines):
		print "LINK STATS HANDLER"
		stuff={}
		section="START"
		for i in range(len(lines)):
			l = lines[i]
			if l == 'Current Rate:':
				stuff['currentDown']=lines[i+1]
				stuff['currentUp']=lines[i+2]
			elif l == 'Max Rate:':
				stuff['maxDown']=lines[i+1]
				stuff['maxUp']=lines[i+2]
			elif l == 'Current Noise Margin':
				stuff['noiseDown']=lines[i+1]
				stuff['noiseUp']=lines[i+2]
			elif l == 'Current Attenuation':
				stuff['attDown']=lines[i+1]
				stuff['attUp']=lines[i+2]
			elif l == 'Current Output Power':
				stuff['powerDown']=lines[i+1]
				stuff['powerUp']=lines[i+2]
			elif l == "ATM":
				section=l
			elif l == "IP":
				section=l
			elif l == 'Transmit:':
				stuff[section + 'xmit']=lines[i+1]
				stuff[section + 'recv']=lines[i+2]
		print stuff

class LinkDetailedStatsHandler(PageHandler):

	def handle(self, lines):
		print "LINK DETAILED STATS HANDLER"
		stuff={}
		for i in range(len(lines)):
			l = lines[i]
			if (i + 4 < len(lines)) and l.endswith(":"):
				stuff[l + "Reset"] = lines[i+1]
				stuff[l + "24Hr"] = lines[i+2]
				stuff[l + "15m"] = lines[i+3]
				stuff[l + "Since"] = lines[i+4]
		print stuff

class DeviceListHandler(PageHandler):

	def isAMacAddress(self, x):
		"""This is a hack to determine whether something is a mac address"""
		rv = 0
		# The gateway prints the mac in the format 01:23:45:67:89:AB
		if len(x) == 17:
			rv = (x[2]==':' and x[5] == ':')
		return rv

	def handle(self, lines):
		print "NETWORK DEVICE LIST HANDLER"
		devices=[]
		for i in range(len(lines)):
			l = lines[i]

			# OK, this is a weird parser, but deal with it
			if (i + 2 < len(lines)) and self.isAMacAddress(lines[i+2]):
				devices.append( (lines[i], lines[i+1], lines[i+2], lines[i+3]))

		print devices

pages=('system_summary.html', 'link_summary.html', 'link_statistics.html',
	'link_detailed_statistics.html', 'network_device_list.html')

handlers={}
for p in pages:
	handlers[p]=PageHandler()
handlers['system_summary.html']=SystemSummaryHandler()
handlers['link_summary.html']=LinkSummaryHandler()
handlers['link_statistics.html']=LinkStatsHandler()
handlers['link_detailed_statistics.html']=LinkDetailedStatsHandler()
handlers['network_device_list.html']=DeviceListHandler()

def processResults(p, body):
	sbody=SpyUtil.deHTML(get.getResponseBodyAsString())
	lines=[]
	st=StringTokenizer(sbody, "\r\n")
	while(st.hasMoreTokens()):
		lines.append(st.nextToken().strip())
	# Remove empty lines
	lines=filter(lambda x: x != '', lines)
	# Call the handler
	handlers[p].handle(lines)

# Main
# get = GetMethod("https://462211000079:syhtob1ZmavwmayI8Y6GRg%3D%3D@208.35.230.144:50001/management/")

Protocol.registerProtocol("https", Protocol("https", MySSLSocketFactory(), 443))
client = HttpClient();
# auth
client.getState().setCredentials("remote", None,
	UsernamePasswordCredentials("462211000079", "syhtob1ZmavwmayI8Y6GRg=="))
client.getState().setAuthenticationPreemptive(1)

timings={}
for i in range(10):
	for p in pages:

		start=time.time()
		get = GetMethod("https://208.35.230.144:50001/management/" + p)
		print "Fetching " + p
		get.setHttp11(1)
		get.setDoAuthentication(1)

		status=client.executeMethod(get)
		processResults(p, get.getResponseBodyAsString())
		get.releaseConnection()
		stop=time.time()

		if not timings.has_key(p):
			timings[p]=[]
		timings[p].append(stop-start)

def add(x,y): return x + y

print ""
print timings
print ""
for k in timings.keys():
	v=timings[k]
	avg=reduce(add, v) / len(v)
	print k + ":  " + `avg`
