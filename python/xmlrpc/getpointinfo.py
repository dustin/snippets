#!/usr/bin/env python
#
# $Id: getpointinfo.py,v 1.1 2002/03/07 02:21:56 dustin Exp $

from sys import argv
import xmlrpclib

server=xmlrpclib.Server('http://bleu.west.spy.net/servlet/net.spy.rpc.XMLRPC');

try:
    a = server.geo.getPointInfo(float(argv[1]), float(argv[2]));

    for poly in a:
        print poly['type'] + ': ' + poly['name']
        if poly['type'] == 'zipcode':
            info=server.zipcodes.lookupZip(int(poly['name']))
            print '\t(city: ' + info['city'] + ')'
except xmlrpclib.Fault, f:
    print f

