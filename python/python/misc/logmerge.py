#!/usr/bin/env python

import gzip
import time
import string
import sys

class LogFile:
	"""Represents an individual log file."""

	def __init__(self, filename):
		"""Initialize a new logfile object."""

		self.__filename=filename
		try:
			self.__input=gzip.open(filename)
			# Get the next line
			self.next()
		except IOError, e:
			if e[0] == 'Not a gzipped file':
				self.__input=open(filename)
				# Get the next line
				self.next()

	def next(self):
		"""Get the next line in this file."""
		self.__currentLine=self.__input.readline()
		if self.__currentLine == '':
			self.__timestamp=None
		else:
			self.__timestamp=self.__getTimestamp()

	def getLine(self):
		"""Get the current line in this file."""
		return(self.__currentLine)

	def getTimestamp(self):
		"""Get the current timestamp."""
		return(self.__timestamp)

	def __getTimestamp(self):
		"""Parse the timestamp"""

		stuff=string.split(self.__currentLine, " ")
		d,t=string.split(stuff[0], 'T')
		y,m,d=string.split(d,'-')
		H,M,S=string.split(t,':')

		return(time.mktime((int(y),int(m),int(d),int(H),int(M),int(S),0,0,-1)))

	def __repr__(self):
		"""Print me"""

		return( "<LogFile " + self.__filename + ">")

class LogMux:
	"""Watch a bunch of LogFile objects and return the records in
	chronological order."""

	def __init__(self):
		"""Initialize."""

		self.__logfiles=[]

	def addLogFile(self, lf):
		"""Add a new logfile to the mux."""
		self.__logfiles.append(lf)

	def next(self):
		"""Get the next log entry"""

		mints=time.time()
		readfrom=None

		for f in self.__logfiles:
			line=f.getLine()
			if line == '':
				sys.stderr.write("### Removing " + repr(f) + "\n")
				self.__logfiles.remove(f)
			else:
				if f.getTimestamp() < mints:
					mints=f.getTimestamp()
					# Remember this one as the minimum timestamp
					readfrom=f

		rv=None
		if readfrom==None:
			rv=''
		else:
			# Get the current line
			rv=readfrom.getLine()
			# Seek to the next line
			readfrom.next()

		return rv

if __name__ == '__main__':

	outfile=gzip.GzipFile(sys.argv[1], 'w')

	lm=LogMux()
	for fn in sys.argv[2:]:
		print "Opening " + fn
		lm.addLogFile(LogFile(fn))

	line=lm.next()
	while line!='':
		outfile.write(line)
		line=lm.next()

	outfile.close()
