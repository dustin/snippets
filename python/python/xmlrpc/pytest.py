#!/usr/bin/env python
#
# $Id: pytest.py,v 1.1 2002/03/08 02:02:35 dustin Exp $

from sys import argv
import xmlrpclib
import hmac

server=xmlrpclib.Server('http://localhost:8000/RPC2')

method = getattr(server, argv[1])
print method(argv[2:], 'other arg')
