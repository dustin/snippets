#!/usr/bin/env python
#
# $Id: pytest.py,v 1.4 2002/12/03 17:49:03 dustin Exp $

from sys import argv
import xmlrpclib
import time

server=xmlrpclib.Server(argv[1])

method = getattr(server, argv[2])
if len(argv)>2:
	print apply(method, argv[3:])
else:
	print method()
