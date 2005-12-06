#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# $Id: snmplib.py,v 1.2 2002/03/28 20:56:20 dustin Exp $
# arch-tag: CACF32D0-228A-4CB7-985F-68C6122E1F95

from __future__ import generators
import sys
from pysnmp import session
from pysnmp import error as snmperror
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

	def walk(self, oid):
		"""Perform an SNMP walk as a generator."""
		# Get the starting OID
		eoids=[self.__convertAndEncode(oid)]
		baseoid=self.oidresolve.lookupOid(oid)
		lastoidvalue=oid

		# Loop
		while 1:
			try:
				# Build the query
				query=self.encode_request('GETNEXTREQUEST', eoids, [])
				# Get the response
				resp=self.send_and_receive(query)
				# decode
				(eoids, values)=self.decode_response(resp)
				oids=map(self.decode_value, eoids)
				values=map(self.decode_value, values)

				# Seems it sometimes doesn't know when to stop
				if oids[0] == lastoidvalue:
					raise StopIteration

				# Don't go outside of the provided base
				if oids[0][0:len(baseoid)] != baseoid:
					raise StopIteration

				yield(oids[0], values[0])

				# Mark it as the last one
				lastoidvalue=oids[0]
			except snmperror.SNMPError, e:
				print "Got an SNMP error:  " + `e`
				# Figure out if we're done, error 2 is NoSuchName
				if e.status == 2:
					raise StopIteration
				else:
					raise snmperror.SNMPError(e.status, e.index)

	def countBranch(self, oid, match=None):
		"""Count the number of entries under the given oid with an optional
		match.

		If match is provided, values of entries must match the given value
		exactly.
		"""
		rv=0
		for pair in self.walk(oid):
			if match==None:
				rv+=1
			else:
				if pair[1]==match:
					rv+=1
		return rv

if __name__ == '__main__':
	s=SnmpSession(sys.argv[1], sys.argv[2])
	print s.get('sysContact.0')
	print s.multiGet(['ifInOctets.1', 'ifOutOctets.1'])

	# for pair in s.walk('mib-2'):
		# print pair

	# All connections
	print s.countBranch('tcpConnState')
	# Only listening
	print s.countBranch('tcpConnState', 2)
