#!/usr/bin/env python

import gzip
import time
import string
import sys
import os
# In-place sorting
import bisect

class LogFile:
	"""Represents an individual log file."""

	def __init__(self, filename):
		"""Initialize a new logfile object."""

		self.__filename=filename
		self.__isOpen=None
		# Open the file to read the initial data
		self.__open()
		# Close it again so we don't have the FD sitting open
		self.__close()

	def __close(self):
		self.__input.close()
		self.__isOpen=None

	def __open(self):
		try:
			self.__input=gzip.open(self.__filename)
		except IOError, e:
			if e[0] == 'Not a gzipped file':
				self.__input=open(self.__filename)
			else:
				raise e
		# Mark it as open
		self.__isOpen=1
		# Get the next line
		self.next()

	def next(self):
		"""Get the next line in this file."""

		# Make sure the file's open
		if self.__isOpen == None:
			sys.stderr.write("# Reopening " + `self` + "\n")
			self.__open()

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

	def __cmp__(self, other):
		"""Compare two objects by timestamp."""
		return(cmp(self.getTimestamp(), other.getTimestamp()))

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
		# Keep it in sequence
		bisect.insort(self.__logfiles, lf)

	def next(self):
		"""Get the next log entry"""

		rv=''

		if len(self.__logfiles) > 0:
			# Remove it from the list
			old=self.__logfiles[0]
			del(self.__logfiles[0])
			# Get the value
			rv=old.getLine()
			# Seek forward
			old.next()
			# Add it back (sorted) if there's more data
			if old.getLine() != '':
				bisect.insort(self.__logfiles, old)
			else:
				sys.stderr.write("### finished " + repr(old) + "\n")

		return rv

if __name__ == '__main__':

	if sys.argv[1] == '-':
		outfile=sys.stdout
	else:
		if os.path.exists(sys.argv[1]):
			raise sys.argv[1] + " already exists."
		outfile=gzip.GzipFile(sys.argv[1], 'w', 3)

	lm=LogMux()
	for fn in sys.argv[2:]:
		sys.stderr.write("Opening " + fn + "\n")
		lm.addLogFile(LogFile(fn))

	line=lm.next()
	while line!='':
		outfile.write(line)
		line=lm.next()

	outfile.close()
