#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
#
# $Id: nntpsucka.py,v 1.2 2002/03/20 10:59:21 dustin Exp $

import nntplib
from nntplib import NNTP
import time
import anydbm
import os

class NNTPSucka:
	headers=['From', 'Subject', 'Message-Id', 'Sender', 'MIME-Version', \
		'Path', 'Newsgroups', 'Organization', 'Approved', 'Sender', \
		'Distribution', \
		'Lines', 'Content-Type', 'Content-Transfer-Encoding']

	def __init__(self, src, dest):
		self.src=src
		self.dest=dest
		self.db=anydbm.open("seen", "c")

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
			+ str(first) + "-" + str(last)
		ids=dict()
		resp, list = self.src.xhdr('message-id', first + "-" + last)
		for set in list:
			ids[set[0]]=set[1]
		for i in range(int(first), int(last)):
			try:
				messid="*empty*"
				try:
					messid=ids[str(i)]
					if self.db.has_key(messid):
						print "Already seen " + messid
					else:
						self.moveArticle(groupname, i)
						self.db[messid]=str(time.time())
				except KeyError, e:
					print "Couldn't find ID in xhdr response."
			except nntplib.NNTPTemporaryError, e:
				print "Failed:  " + str(e)

	def copyServer(self):
		resp, list = self.dest.list()
		for l in list:
			group=l[0]
			try:
				if group.find("binaries") >= 0:
					print "Skipping " + group
				else:
					print "Copying " + group
					self.copyGroup(group)
			except nntplib.NNTPTemporaryError, e:
				print "Error on group " + group + ":  " + str(e)

def main():
	s=NNTP('news.mindspring.com')
	d=NNTP('news.west.spy.net')
	sucka=NNTPSucka(s,d)

	sucka.copyServer()

if __name__ == '__main__':
	main()
