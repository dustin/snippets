#!/usr/bin/env jython

import com
import java

import time

import lookupSerial

myserialnumber='400000000334'

class ClientThread(java.lang.Runnable):
    def __init__(self, conn, port=4721):
        self.conn=conn
        self.port=port

    def run(self):
        print "connecting to server on port " + `self.port`

        s=java.net.Socket(java.net.InetAddress.getLocalHost(), self.port)
        self.conn.handleConnection(s.getInetAddress().getHostAddress(),
            s.getInputStream(), s.getOutputStream())

class MuxHandler(com.twowire.protocol.xmlrpcmux.XmlRpcChannel):

    def __init__(self):
        pass

    def handleChannel(self, channel):
        print "Handling " + `channel` + " named " + `channel.getName()`
        com.twowire.protocol.xmlrpcmux.XmlRpcChannel.handleChannel(self,
            channel)

class XMLRPCTest:
    def __init__(self):
        # self.client=com.twowire.protocol.xmlrpcmux.XmlRpcMuxConnection(
            # myserialnumber, "ba0110de16993f96", None, 1, MuxHandler())
        self.client=com.twowire.protocol.xmlrpcmux.XmlRpcMuxConnection(
            myserialnumber, "ba0110de16993f96", None, 1, MuxHandler())

    def go(self):
        t=java.lang.Thread(ClientThread(self.client, 9898))
        t.setDaemon(1)
        t.start()

        # Bring up the client
        self.client.bringUp()

        cms_rpc = com.twowire.rpc.RpcManager.getCMSRpcAPI()
        cms_management = com.twowire.rpc.RpcManager.getCMSManagementAPI()

        print "Version=" + `cms_rpc.GetRPCVersion()`

        print "Methods:"
        for m in cms_rpc.GetRPCMethods():
            print "\t" + m

        gws=lookupSerial.GatewaySerial(myserialnumber)

        print "Heartbeat:"
        cms_management.Heartbeat(myserialnumber, 1,
            gws.getHeartbeatStruct('event', 'command'))
        print "Hearbeat complete."

        print "Requesting download."
        cms_management.RequestDownload(myserialnumber, "type", "arg")
        print "Download request complete."

        print "Sleeping..."
        time.sleep(60)
        print "Slept."

if __name__ == '__main__':
    x=XMLRPCTest()
    x.go()
