#!/usr/bin/env jython

# Optional debug
# -Dorg.apache.commons.logging.simplelog.showdatetime=true \
# -Dorg.apache.commons.logging.Log=org.apache.commons.logging.impl.SimpleLog \
# -Dorg.apache.commons.logging.simplelog.log.httpclient.wire=trace \
# -Dorg.apache.commons.logging.simplelog.log.org.apache.commons.httpclient=debug \

import jarray

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

# Main

Protocol.registerProtocol("https", Protocol("https", MySSLSocketFactory(), 443))
client = HttpClient();
# auth
# client.getState().setAuthenticationPreemptive(1)
client.getState().setCredentials("remote", "208.35.230.144",
	UsernamePasswordCredentials("462211000079", "syhtob1ZmavwmayI8Y6GRg=="))

get = GetMethod("https://208.35.230.144:50001/management/")
# get = GetMethod("https://462211000079:syhtob1ZmavwmayI8Y6GRg%3D%3D@208.35.230.144:50001/management/")
get.setHttp11(1)
get.setDoAuthentication(1)

status=client.executeMethod(get)

print get.getResponseBodyAsString()
# print "Auth realm:  " + str(get.getAuthenticationRealm())
get.releaseConnection()
