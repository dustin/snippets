#!/usr/bin/env python
#
# $Id: nmapparser.py,v 1.1 2002/03/20 08:15:24 dustin Exp $
# arch-tag: 5D4A22FC-C946-458F-B5E3-D5C1FC194975

from sys import argv

class NmapPort:

	def __init__(self, entry):
		a=entry.strip().split('/')
		self.port=int(a[0])
		self.status=a[1]
		self.proto=a[2]

	def getPort(self):
		return self.port

	def getStatus(self):
		return self.status

	def getProto(self):
		return self.proto

	def __str__(self):
		return("Port:  " + str(self.port) + "/" + self.proto \
			+ " (" + self.status + ")")

class NmapEntry:

	def __init__(self, line):
		a=line.strip().split('\t')
		parts=dict()
		for p in a:
			t=p.split(':', 2)
			parts[t[0].strip()]=t[1].strip()
		# Get the IP address.
		self.ip=parts['Host'].split(' ')[0]
		# Get the ports
		self.ports=list()
		if parts.has_key('Ports'):
			for i in parts['Ports'].split(','):
				self.ports.append(NmapPort(i))

	def getAddress(self):
		return self.ip

	def getPorts(self):
		return self.ports

	def __str__(self):
		return "NmapEntry from " + self.ip + " (" + str(len(self.ports)) \
			+ " entries)"

	def __len__(self):
		return len(self.ports)

	def __iter__(self):
		return iter(self.ports)

def loadNmap(filename):
	entries=list()
	f=file(filename)
	for l in f.readlines():
		if l[0] != '#':
			entries.append(NmapEntry(l))
	f.close()
	return entries

def main():
	for e in loadNmap(argv[1]):
		print e

if __name__ == '__main__':
	main()
