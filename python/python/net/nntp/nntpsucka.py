#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
#
# $Id: nntpsucka.py,v 1.7 2002/03/20 19:47:27 dustin Exp $

import nntplib
from nntplib import NNTP
import time
import anydbm
import os

class Stats:
	def __init__(self):
		self.moved=0
		self.dup=0
		self.other=0

	def addMoved(self):
		self.moved+=1

	def addDup(self):
		self.dup+=1

	def addOther(self):
		self.other+=1

	def __str__(self):
		return "Moved:  " + str(self.moved) \
			+ ", duplicate:  " + str(self.dup) \
			+ ", Other:  " + str(self.other)

class NNTPSucka:
	headers=['From', 'Subject', 'Message-Id', 'Sender', 'MIME-Version', \
		'Path', 'Newsgroups', 'Organization', 'Approved', 'Sender', \
		'Distribution', \
		'Lines', 'Content-Type', 'Content-Transfer-Encoding']

	def __init__(self, src, dest):
		self.src=src
		self.dest=dest
		self.db=anydbm.open("seen", "c")
		self.stats=Stats()

	def __del__(self):
		self.db.close()

	def headerMatches(self, h):
		rv=None
		for header in self.headers:
			if h.lower().find(header.lower()) == 0:
				rv=1
		return rv

	def moveArticle(self, groupname, which):
		print "Moving " + str(which)
		resp, nr, id, lines = self.src.article(str(which))
		resp = self.dest.shortcmd('POST')
		if resp[0] != '3':
			raise nntplib.NNTPReplyError(resp)
		headers=1
		for l in lines:
			if l == '':
				headers=None
				self.dest.putline('')
			else:
				if headers:
					if self.headerMatches(l):
						self.dest.putline(l)
				else:
					if l == '.':
						print "*** L was ., adding a dot. ***"
						l = '..'
					self.dest.putline(l)
		self.dest.putline('.')
		self.dest.getresp()
		print "Posted."

	def copyGroup(self, groupname):
		self.dest.group(groupname)
		resp, count, first, last, name = self.src.group(groupname)
		print "Copying " + str(count) + " articles:  " \
			+ str(first) + "-" + str(last) + " in " + groupname
		ids=dict()
		resp, list = self.src.xhdr('message-id', first + "-" + last)
		for set in list:
			ids[set[0]]=set[1]
		for i in range(int(first), int(last)):
			try:
				messid="*empty*"
				messid=ids[str(i)]
				if self.db.has_key(messid):
					# Too noisy, but kind of interesting.
					# print "Already seen " + messid
					self.stats.addDup()
				else:
					self.moveArticle(groupname, i)
					self.db[messid]=str(time.time())
					self.stats.addMoved()
			except KeyError, e:
				# Couldn't find the header, article probably doesn't
				# exist anymore.
				pass
			except nntplib.NNTPTemporaryError, e:
				# Save it if it's duplicate
				if str(e).find("Duplicate"):
					self.db[messid]=str(time.time())
					self.stats.addDup()
				else:
					self.stats.addOther()
				print "Failed:  " + str(e)

	def copyServer(self):
		resp, list = self.dest.list()
		for l in list:
			group=l[0]
			try:
				self.copyGroup(group)
			except nntplib.NNTPTemporaryError, e:
				print "Error on group " + group + ":  " + str(e)

	def getStats(self):
		return self.stats

def main():
	s=NNTP('news.mindspring.com')
	d=NNTP('news.west.spy.net')
	sucka=NNTPSucka(s,d)

	sucka.copyServer()

	print sucka.getStats()

if __name__ == '__main__':
	main()
