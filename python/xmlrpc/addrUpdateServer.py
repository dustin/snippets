#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: A20F8936-3AC2-11D9-A224-000A957659CC

import os
import sys
import logging
import xmlrpclib
import SimpleXMLRPCServer

IPF_IN="ipf.conf"
IPF_OUT="ipf.conf.tmp"
LOGFILE="ipf.log"
LOGFORMAT="%(asctime)s %(name)s:%(levelname)s %(message)s"

class CmdError(xmlrpclib.Fault):
    def __init__(self, s, rv):
        xmlrpclib.Fault.__init__(self, rv, s)

class MyXMLRPCServer(SimpleXMLRPCServer.SimpleXMLRPCServer):

    allow_reuse_address = True

    def __init__(self, addr):
        SimpleXMLRPCServer.SimpleXMLRPCServer.__init__(self, addr,
            logRequests=False)

def fixAddr(a):
    a, b, c, x=a.split(".")
    return a + "." + b + "." + c + ".0/24"

def runCmd(cmd):
    logging.debug("Running %s", cmd)
    rv=os.system(cmd)
    if rv != 0:
        ce=CmdError(cmd, rv)
        logging.warn("Command failed:  %s", ce)
        raise ce

def runUpdate(pattern):
    logging.info("Updating phone address to %s", pattern)
    runCmd("/sbin/ipf -Fa -I " + IPF_OUT)
    runCmd("/sbin/ipf -s")

def updateAddress(addrIn):
    addr=fixAddr(addrIn)
    fin=open(IPF_IN)
    fout=open(IPF_OUT, "w")

    for l in fin:
        if l.find("@PHONEADDRESS@") > 0:
            fout.write(l[2:].replace("@PHONEADDRESS@", addr))
        else:
            fout.write(l)

    fin.close()
    fout.close()

    runUpdate(addr)

    return "Updated"

if __name__ == '__main__':
    # Logging config
    hdlr=logging.FileHandler(LOGFILE, "a")
    hdlr.setFormatter(logging.Formatter(LOGFORMAT))
    logging.root.addHandler(hdlr)
    logging.root.setLevel(logging.DEBUG)

    server = MyXMLRPCServer(("0.0.0.0", 8000))
    server.register_function(updateAddress, 'updateAddress')
    server.serve_forever()
