#!/usr/bin/env python
"""

Create a set of filelists that will each be less than 650MB.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: chunkify.py,v 1.1 2002/04/07 07:56:33 dustin Exp $
"""

import os, sys

class Chunker:

	MAXSIZE=(650*1024*1024)

	def __init__(self, filebase):
		self.currentSize=-1
		self.currentFileId=0
		self.filebase=filebase
		self.currentFile=None
		self.newfile()

	def newfile(self):
		if self.currentSize==0:
			raise "Requested newfile with a size of 0"
		self.currentSize=0
		if self.currentFile:
			self.currentFile.close()
		self.currentFileId+=1
		fn=self.filebase + "." + str(self.currentFileId)
		self.currentFile=file(fn, 'w')

	def storeFile(self, path):
		size=os.path.getsize(path)
		print path + " is a file of " + str(size) + " bytes."
		if self.currentSize + size > self.MAXSIZE:
			self.newfile()
		self.currentSize+=size
		self.currentFile.write(path + "\n")

	def processThing(self, path):
		print "Processing " + path
		if(os.path.isdir(path)):
			for f in os.listdir(path):
				self.processThing(os.path.join(path, f))
		else:
			self.storeFile(path)

if __name__ == '__main__':
	startdir=sys.argv[2]
	chunker=Chunker(sys.argv[1])
	chunker.processThing(startdir)
