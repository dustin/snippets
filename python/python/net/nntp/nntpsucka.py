#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
#
# $Id: nntpsucka.py,v 1.10 2002/03/20 20:01:30 dustin Exp $

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

######################################################################

class NewsDB:
	def __init__(self):
		self.db=anydbm.open("newsdb", "c")

	def hasArticle(self, message_id):
		return self.db.has_key("a/" + message_id)

	def markArticle(self, message_id):
		self.db["a/" + message_id] = str(time.time())

	def getLastId(self, group):
		rv=0
		try:
			rv=self.db["l/" + group]
		except KeyError:
			pass
		return rv

	def setLastId(self, group, id):
		self.db["l/" + group]=id

	def __del__(self):
		self.db.close()

######################################################################

class NNTPSucka:
	headers=['From', 'Subject', 'Message-Id', 'Sender', 'MIME-Version', \
		'Path', 'Newsgroups', 'Organization', 'Approved', 'Sender', \
		'Distribution', \
		'Lines', 'Content-Type', 'Content-Transfer-Encoding']

	def __init__(self, src, dest):
		self.src=src
		self.dest=dest
		self.db=NewsDB()
		self.stats=Stats()

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
		ids=dict()
		resp, list = self.src.xhdr('message-id', first + "-" + last)
		for set in list:
			ids[set[0]]=set[1]
		myfirst=self.db.getLastId(groupname)
		if int(myfirst) < int(first):
			myfirst=first
		mycount=(int(last)-int(myfirst))
		print "Copying " + str(mycount) + " articles:  " \
			+ str(myfirst) + "-" + str(last) + " in " + groupname
		for i in range(int(myfirst), int(last)):
			try:
				messid="*empty*"
				messid=ids[str(i)]
				if self.db.hasArticle(messid):
					print "Already seen " + messid
					self.stats.addDup()
				else:
					self.moveArticle(groupname, i)
					self.db.markArticle(messid)
					self.stats.addMoved()
			except KeyError, e:
				# Couldn't find the header, article probably doesn't
				# exist anymore.
				pass
			except nntplib.NNTPTemporaryError, e:
				# Save it if it's duplicate
				if str(e).find("Duplicate"):
					self.db.markArticle(messid)
					self.stats.addDup()
				else:
					self.stats.addOther()
				print "Failed:  " + str(e)
		self.db.setLastId(groupname, last)

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
