#!/usr/bin/env python

import zipfile
from sys import argv
import os
import string

import TigerTypes

class Counter:
	def __init__(self):
		pass

	def count(self, fn):
		rv=0
		zf=zipfile.ZipFile(fn)
		bytype=dict()
		for file in zf.namelist():
			zi=zf.getinfo(file)
			t=TigerTypes.getType(file[-1])
			nr=zi.file_size/(t.recordSize()+2)
			bytype[file[-1]]=nr
			rv = rv + nr
		return rv, bytype

def main():
	totalcount=0

	counter = Counter()
	for d in argv[1:]:
		print "Looking in " + d
		for ftmp in os.listdir(d):
			if string.find(ftmp, ".zip") >= 0:
				f= d + "/" + ftmp
				print "Looking at " + f
				count=counter.count(f)
				totalcount = totalcount + count[0]

	print "Need to load " + str(totalcount) + " records."

if __name__ == '__main__':
	main()
