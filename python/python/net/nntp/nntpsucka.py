#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
#
# $Id: nntpsucka.py,v 1.25 2002/03/26 20:10:29 dustin Exp $

import nntplib
import time
import anydbm
import signal
import os
import sys

# My pidlock
import pidlock

class Stats:
	"""Keep statistics describing what got moved."""

	def __init__(self):
		self.moved=0
		self.dup=0
		self.other=0

	def addMoved(self):
		"""Mark one article copied."""
		self.moved+=1

	def addDup(self):
		"""Mark one article duplicate (not copied)."""
		self.dup+=1

	def addOther(self):
		"""Mark one article not copied, but not duplicate."""
		self.other+=1

	def __str__(self):
		return "Moved:  " + str(self.moved) \
			+ ", duplicate:  " + str(self.dup) \
			+ ", Other:  " + str(self.other)

######################################################################

class NewsDB:
	"""Database of seen articles and groups.

	Entries for groups will have keys beginning with ``l/'' and entries for
	individual articles will have keys beginning with ``a/''

	Article entries (a) will have a string value that's a floating point number
	indicating the time that this article was copied.

	Group entries (l) have a string value that represents that last seen
	article by number for the given group (the part after the l/).
	"""
	def __init__(self):
		self.db=anydbm.open("newsdb", "c")

	def hasArticle(self, message_id):
		"""Return true if there is a reference to the given message ID in
		this database.
		"""
		return self.db.has_key("a/" + message_id)

	def markArticle(self, message_id):
		"""Mark the article seen."""
		self.db["a/" + message_id] = str(time.time())

	def getLastId(self, group):
		"""Get the last seen article ID for the given group, or 0 if this
		group hasn't been seen.
		"""
		rv=0
		try:
			rv=self.db["l/" + group]
		except KeyError:
			pass
		return rv

	def setLastId(self, group, id):
		"""Set the last seen article ID for the given group."""
		self.db["l/" + group]=id

	def __del__(self):
		"""Close the DB on destruct."""
		self.db.close()

	def getGroupRange(self, group, first, last):
		"""Get the group range for the given group.

		The arguments represent the group you're looking to copy, and the
		first and last article numbers as provided by the news server.

		The first, last, and count that should be checked will be returned
		as a tuple."""
		myfirst=self.getLastId(group)
		if (int(myfirst) < int(first)) or (int(myfirst) > int(last)):
			myfirst=first
		mycount=(int(last)-int(myfirst))

		return myfirst, last, mycount

######################################################################

class NNTPClient(nntplib.NNTP):
	"""An extension of nntplib.NNTP suitable for...well, it actually
	works."""

	headers=['From', 'Subject', 'Message-Id', 'Sender', 'MIME-Version', \
		'Path', 'Newsgroups', 'Organization', 'Approved', 'Sender', \
		'Distribution', \
		'Lines', 'Content-Type', 'Content-Transfer-Encoding']

	def __init__(self, host, port=119,user=None,password=None,readermode=None):
		"""See netlib.NNTP"""
		nntplib.NNTP.__init__(self, host, port, user, password, readermode)
		self.checkMode()
		# self.debugging=1

	def checkMode(self):
		"""Upon construct, this tries to figure out whether the server
		considers us a feeder or a reader."""
		try:
			self.group('control')
			self.currentmode='reader'
		except nntplib.NNTPPermanentError:
			self.currentmode='poster'

	def __headerMatches(self, h):
		"""Internal, checks to see if the header ``h'' is in our list of
		approved headers."""
		rv=None
		for header in self.headers:
			if h.lower().find(header.lower()) == 0:
				rv=1
		return rv

	def ihave(self, id):
		"""Send an IHAVE for a message ID (feeder mode only).

		An exception will be raised if something breaks, or the article
		isn't wanted.  If an exception is not raised, follow with a stream
		of the article."""
		print "IHAVing " + id
		resp = self.shortcmd('IHAVE ' + id)
		print "IHAVE returned " + str(resp)

	def copyArticle(self, src, which, messid):
		"""Copy an article from the src server to this server.

		which is the ID of the message on the src server, messid is the
		message ID of the article."""
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
		"""Stream an article to this server."""
		print "*** TAKE THIS! ***"
		for l in lines:
			if l == '.':
				print "*** L was ., adding a dot. ***"
				l = '..'
			self.putline(l)
		self.putline('.')
		self.getresp()

	def post(self, lines):
		"""Post an article to this server."""
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
	"""Copy articles from one NNTP server to another."""

	def __init__(self, src, dest):
		"""Get an NNTPSucka with two NNTPClient objects representing the
		source and destination."""
		self.src=src
		self.dest=dest
		self.db=NewsDB()
		self.stats=Stats()

	def copyGroup(self, groupname):
		"""Copy the given group from the source server to the destination
		server.

		Efforts are made to ensure only articles that haven't been seen are
		copied."""
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
		"""Copy all groups that appear on the destination server to the
		destination server from the source server."""
		resp, list = self.dest.list()
		for l in list:
			group=l[0]
			try:
				self.copyGroup(group)
			except nntplib.NNTPTemporaryError, e:
				print "Error on group " + group + ":  " + str(e)

	def getStats(self):
		"""Get the statistics object."""
		return self.stats

class Timeout:
	"""This is an exception that's raised when the alarm handler fires."""
	pass

def alarmHandler(sig, frame):
	"""Do nothing but raise a timeout."""
	raise Timeout

def main():
	# Lock.
	lock=pidlock.PidLock("nntpsucka.pid")

	signal.signal(signal.SIGALRM, alarmHandler)
	signal.alarm(3500)

	sucka=None
	try:
		start=time.time()
		s=NNTPClient(sys.argv[1])
		d=NNTPClient(sys.argv[2])
		sucka=NNTPSucka(s,d)
		sucka.copyServer()
		stop=time.time()
	except IndexError:
		sys.stderr.write("Usage:  " + sys.argv[0] + " srchost desthost.\n")
	except Timeout:
		sys.stderr.write("Took too long.\n")

	if sucka:
		print sucka.getStats()
		sys.stderr.write("Total time spent:  " + str(stop-start) + "s\n")

if __name__ == '__main__':
	try:
		main()
	except pidlock.AlreadyLockedException, ale:
		sys.stderr.write("Already running:  " + str(ale[0]))
