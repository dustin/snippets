#!/usr/bin/env jython

import java
import com

class XMLRPCTest:

	def __init__(self):
		"""Get ready for some calls."""
		self.__setup()

	def __setup(self):
		"""Perform setup required to make these calls."""
		s=java.net.Socket(java.net.InetAddress.getByName("localhost"), 4269)
		client=com.twowire.protocol.xmlrpc.BaseXmlRpcClient(
			s.getInputStream(), s.getOutputStream())
		com.twowire.protocol.xmlrpc.XmlRpcManager.registerClientFactory(
			com.twowire.test.junit.rpc.TestLocalhostRpcClientFactory())
		self.cms_rpc=com.twowire.rpc.RpcManager.getCMSRpcAPI()

if __name__=='__main__':
	t=XMLRPCTest()

	print t.cms_rpc.GetRPCVersion()
	print t.cms_rpc.GetRPCMethods()
