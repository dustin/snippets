#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
#
# $Id: nntpsucka.py,v 1.22 2002/03/24 09:33:07 dustin Exp $

import nntplib
import time
import anydbm
import signal
import os

# My pidlock
import pidlock

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

	def getGroupRange(self, group, first, last):
		myfirst=self.getLastId(group)
		if (int(myfirst) < int(first)) or (int(myfirst) > int(last)):
			myfirst=first
		mycount=(int(last)-int(myfirst))

		return myfirst, last, mycount

######################################################################

class NNTPClient(nntplib.NNTP):

	headers=['From', 'Subject', 'Message-Id', 'Sender', 'MIME-Version', \
		'Path', 'Newsgroups', 'Organization', 'Approved', 'Sender', \
		'Distribution', \
		'Lines', 'Content-Type', 'Content-Transfer-Encoding']

	def __init__(self, host, port=119,user=None,password=None,readermode=None):
		nntplib.NNTP.__init__(self, host, port, user, password, readermode)
		self.checkMode()
		# self.debugging=1

	def checkMode(self):
		try:
			self.group('control')
			self.currentmode='reader'
		except nntplib.NNTPPermanentError:
			self.currentmode='poster'

	def __headerMatches(self, h):
		rv=None
		for header in self.headers:
			if h.lower().find(header.lower()) == 0:
				rv=1
		return rv

	def ihave(self, id):
		print "IHAVing " + id
		resp = self.shortcmd('IHAVE ' + id)
		print "IHAVE returned " + str(resp)

	def copyArticle(self, src, which, messid):
		print "Moving " + str(which)
		if self.currentmode == 'reader':
			resp, nr, id, lines = src.article(str(which))
			self.post(lines)
		else:
			self.ihave(messid)
			try:
				resp, nr, id, lines = src.article(str(which))
			except nntplib.NNTPTemporaryError, e:
				# Generate an error, I don't HAVE this article, after all
				self.shortcmd('.')
			self.takeThis(messid, lines)

	def takeThis(self, messid, lines):
		print "*** TAKE THIS! ***"
		# self.putline('TAKETHIS: ' + messid)
		for l in lines:
			if l == '.':
				print "*** L was ., adding a dot. ***"
				l = '..'
			self.putline(l)
		self.putline('.')
		self.getresp()

	def post(self, lines):
		print "*** POSTING! ***"
		resp = self.shortcmd('POST')
		if resp[0] != '3':
			raise nntplib.NNTPReplyError(resp)
		headers=1
		for l in lines:
			if l == '':
				headers=None
				self.putline('')
			else:
				if headers:
					if self.__headerMatches(l):
						self.putline(l)
				else:
					if l == '.':
						print "*** L was ., adding a dot. ***"
						l = '..'
					self.putline(l)
		self.putline('.')
		self.getresp()

######################################################################

class NNTPSucka:

	def __init__(self, src, dest):
		self.src=src
		self.dest=dest
		self.db=NewsDB()
		self.stats=Stats()

	def copyGroup(self, groupname):
		resp, count, first, last, name = self.src.group(groupname)

		# Figure out where we are
		myfirst, mylast, mycount= self.db.getGroupRange(groupname, first, last)
		print "Copying " + str(mycount) + " articles:  " \
			+ str(myfirst) + "-" + str(mylast) + " in " + groupname

		# Grab the IDs
		resp, list = self.src.xhdr('message-id', \
			str(myfirst) + "-" + str(mylast))
		ids=dict()
		for set in list:
			ids[set[0]]=set[1]

		for i in range(int(myfirst), int(mylast)):
			try:
				messid="*empty*"
				messid=ids[str(i)]
				if self.db.hasArticle(messid):
					print "Already seen " + messid
					self.stats.addDup()
				else:
					self.dest.copyArticle(self.src, i, messid)
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

class Timeout:
	pass

def alarmHandler(sig, frame):
	raise Timeout

def main():
	# Lock.
	lock=pidlock.PidLock("nntpsucka.pid")

	signal.signal(signal.SIGALRM, alarmHandler)
	signal.alarm(3500)

	try:
		start=time.time()
		s=NNTPClient('news.mindspring.com')
		d=NNTPClient('news.west.spy.net')
		sucka=NNTPSucka(s,d)
		sucka.copyServer()
		stop=time.time()
	except Timeout:
		print "Took too long."

	print sucka.getStats()
	print "Total time spent:  " + str(stop-start) + "s"

if __name__ == '__main__':
	main()
