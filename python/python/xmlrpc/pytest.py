#!/usr/bin/env python
#
# $Id: pytest.py,v 1.3 2002/04/05 03:27:01 dustin Exp $

from sys import argv
import xmlrpclib
import time

server=xmlrpclib.Server('http://localhost:' + argv[1] + '/RPC2')

method = getattr(server, argv[2])
if len(argv)>2:
	print apply(method, argv[3:])
else:
	print method()
