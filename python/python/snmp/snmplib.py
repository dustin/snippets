#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# $Id: snmplib.py,v 1.1 2002/03/28 05:51:46 dustin Exp $

from pysnmp import session
import mib2

class SnmpSession(session.session):
	"""A wrapper around pysnmp to make it more usable in applications."""

	oidresolve=mib2.OidResolve()

	def __init__(self, host, community='public'):
		"""Get a session on the given host using the given community."""
		session.session.__init__(self, host, community)

	def __convertAndEncode(self, oid):
		return(self.encode_oid(self.str2nums(self.oidresolve.lookupOid(oid))))

	def multiGet(self, oids):
		"""Get a few variables simultaneously from this session."""
		# Encode the oids
		oids=map(self.__convertAndEncode, oids)
		# Build the query
		query=self.encode_request('GETREQUEST', oids, [])
		# Perform the query
		resp=self.send_and_receive(query)
		# Decode the data
		(oids, values)=self.decode_response(resp)
		oids=map(self.decode_value, oids)
		values=map(self.decode_value, values)

		return(oids, values)

	def get(self, oid):
		"""Get a single variable from an SNMP session."""
		(oids, values)=self.multiGet([oid])
		return(values[0])

if __name__ == '__main__':
	s=SnmpSession('butterfly', 'public')
	print s.get('sysContact.0')
	print s.multiGet(['ifInOctets.1', 'ifOutOctets.1'])
