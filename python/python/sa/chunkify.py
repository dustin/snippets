#!/usr/bin/env python
"""

Create a set of filelists that will each be less than 650MB.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: chunkify.py,v 1.3 2002/04/07 10:29:32 dustin Exp $
"""

import os, sys
import zipfile
import getopt
import exceptions

class UsageError(exceptions.Exception):
	"""Exception thrown for an invalid usage."""
	pass

class Chunker:

	MAXSIZE=(650*1024*1024)

	def __init__(self, filebase, makezip=None, maxsize=MAXSIZE):
		"""Get a chunker using the given base filenames for list files.

		Optionally, supply a maximum size for each file, and choose between
		whether to make a zip file or just a list of the files."""
		# Arguments
		self.filebase=filebase
		self.makezip=makezip
		self.maxsize=maxsize

		# Accumulated size of the current file
		self.currentSize=-1
		# Current file ID (1, 2, etc...)
		self.currentFileId=0
		# Current file (zipfile or list file)
		self.currentFile=None
		# New file callback
		self.nfcallback=None
		# Now, go fetch a new file
		self.__newfile()

	def registerNewFileCallback(self, cb):
		"""Register a callback function to be called each time a new file
		is required.

		The callback will be called with the Chunker instance and the new
		filename as arguments.

		Since the first file is created upon construction of an object, the
		callback may only be called beginning with the creation of the
		second file."""
		self.nfcallback=cb

	def __newfile(self):
		if self.currentSize==0:
			raise "Requested newfile with a size of 0"
		# Reset the file size to zero
		self.currentSize=0
		# If there's an open file already, close it
		if self.currentFile:
			self.currentFile.close()
		# Move on to the next file.
		self.currentFileId+=1
		# Create the base filename
		fn=self.filebase + "." + str(self.currentFileId)
		# Append .zip if it's a zip file
		if self.makezip:
			fn+='.zip'
		# Perform the callback (if any)
		if self.nfcallback!=None:
			self.nfcallback(self, fn)
		# Create either a zip file, or regular file, depending on what the
		# caller wants
		if self.makezip:
			self.currentFile=zipfile.ZipFile(fn, 'w')
		else:
			self.currentFile=file(fn, 'w')

	def __storeFile(self, path):
		# Get the size of the file
		size=os.path.getsize(path)
		# # print path + " is a file of " + str(size) + " bytes."
		# Figure out if we need a new file
		if self.currentSize + size > self.maxsize:
			self.__newfile()
		# Increment the size
		self.currentSize+=size
		# If we're just making a list, append a newline to the filename
		if not self.makezip:
			path+='\n'
		# Store the file.
		self.currentFile.write(path)

	def process(self, path):
		"""Process the given path."""
		print "Processing " + path
		if(os.path.isdir(path)):
			for f in os.listdir(path):
				self.process(os.path.join(path, f))
		else:
			try:
				self.__storeFile(path)
			except OSError, e:
				print e

def pauseCallback(chunker, fn):
	print "--- Press enter to create " + fn + " ---"
	sys.stdin.readline()

def main():
	try:
		opts, args = getopt.getopt(sys.argv[1:], 'c:m:zp')
	except getopt.GetoptError, e:
		raise UsageError(e[1])

	chunkname=None
	makezip=None
	maxsize=None
	dopause=None

	for pair in opts:
		if pair[0]=='-c':
			chunkname=pair[1]
		elif pair[0]=='-z':
			makezip=1
		elif pair[0]=='-m':
			maxsize=int(pair[1])
		elif pair[0]=='-p':
			dopause=1

	startdir=args[0]
	print "Starting from " + startdir
	chunker=Chunker(chunkname, makezip, maxsize)
	if dopause:
		chunker.registerNewFileCallback(pauseCallback)
	chunker.process(startdir)

def usage():
	print "Usage:  " + sys.argv[0] \
		+ " -c basefile [-m chunksize] [-z] startpath"
	print "\t-c specifies the base name for each file"
	print "\t-m specifies the chunk size (in bytes) for each file " \
		"(default, 650MB)"
	print "\t-z chooses to make zip files rather than file lists."
	print "\tstartpath is the directory to traverse."

if __name__ == '__main__':
	try:
		main()
	except UsageError:
		usage()
