#!/usr/bin/env python

import imaplib
import time
import getpass
import psycopg
import string
import exceptions
import stemmer

class Headers:

	def __init__(self, headers):
		self.parts=[]
		for l in headers.split('\r\n'):
			if l.find('\t') == 0:
				try:
					self.parts[-1][1]+= '\r\n' + l
				except IndexError, e:
					print "*** Weird header (ignoring):  " + l
			else:
				self.parts.append(l.split(': ', 2))

	def __getitem__(self, which):
		rv=[]
		which=which.lower()
		for p in self.parts:
			if p[0].lower() == which:
				rv.append(p[1])
		return rv

	def __repr__(self):
		return(repr(self.parts))

class InvalidBox(exceptions.Exception):
	pass

class DuplicateMessage(exceptions.Exception):
	pass

class ImapIndex:
	"""IMAP indexer."""

	def __init__(self, imap):
		self.ps=stemmer.PorterStemmer()
		self.imap=imap
		self.db=psycopg.connect('dbname=imapindex host=db user=dustin ' + \
			'password=blahblah', serialize=0)
		self.c=self.db.cursor()

	def cleanWord(self, w):
		w=w.lower()
		# goodchars=string.lowercase.split('')
		goodchars=string.lowercase
		while len(w)>0 and w[0] not in goodchars:
			w=w[1:]
		while len(w)>0 and w[-1] not in goodchars:
			w=w[0:-1]
		# Get rid of certain known bad things
		if w.find(">") >= 0:
			w=''
		elif w.find("<") >= 0:
			w=''
		elif w.find("&") >= 0:
			w=''
		# I'd really like to do something with this.
		elif w.find("...") >= 0:
			w=''
		if len(w)>30 or len(w)<3:
			w=None
		else:
			w=self.ps.stem(w, 0, len(w)-1)
		return w

	def countFrequency(self, uid, message):
		count=dict()
		for w in message.split():
			w=self.cleanWord(w)
			if w:
				if count.has_key(w):
					count[w]+=1
				else:
					count[w]=1
		return count

	def _getBox(self, box):
		self.c.execute('select folder_id from folders ' \
			+ 'where folder_name=%(name)s', {'name': box})
		rv=self.c.fetchall()
		if len(rv)!=1:
			raise InvalidBox, box
		return rv[0][0]

	def _createBox(self, box):
		self.c.execute('insert into folders(folder_name) ' \
			+ 'values(%(name)s)', {'name': box})
		self.c.commit()
		self.c.execute("select currval('folders_folder_id_seq')")
		rv=self.c.fetchall()
		if len(rv)!=1:
			raise InvalidBox, box
		return rv[0][0]

	def _getMessage(self, boxid, headers):
		"""Get a new ID for the message represented by the given headers."""

		messid=headers['Message-ID'][0]
		from_line=''
		try:
			from_line=headers['from'][0]
		except IndexError:
			pass

		to_line=''
		try:
			to_line=headers['to'][0]
		except IndexError:
			pass

		subject_line=''
		try:
			subject_line=headers['subject'][0]
		except IndexError:
			pass

		try:
			self.c.execute('insert into messages(folder_id, messid, ' \
				+ 'from_line, to_line, subject_line, timestamp)\n' \
				+ '\tvalues(%(boxid)i, %(messid)s, %(from)s, %(to)s, ' \
				+ '%(subject)s, now())',
				{ 'boxid': boxid, 'messid': messid, 'from': from_line,
				'to': to_line, 'subject': subject_line})
		except psycopg.ProgrammingError, e:
			if str(e).find('messages_bymid') >= 0:
				raise DuplicateMessage, messid
			else:
				raise e
		self.c.execute("select currval('messages_message_key_seq')")
		rv=self.c.fetchall()
		return rv[0][0]

	def store(self, boxid, headers, freq):
		"""Store a message."""

		messkey=self._getMessage(boxid, headers)
		try:
			print "From " + `headers['from'][0]` \
				+ ' - ' + `headers['subject'][0]` + "..."
		except IndexError:
			print "From " + `headers['from']` \
				+ ' - ' + `headers['subject']` + "..."
		for k in freq:
			self.c.execute(
				'insert into keywords(keyword, message_key, keycount)\n' \
				+ '\tvalues(%(word)s, %(message)i, %(count)i)',
				{ 'word': k, 'message': messkey, 'count': freq[k]})
		self.c.commit()
		print "Done."

	def indexBox(self, box):
		"""Index an IMAP box."""
		print "Indexing " + box
		rc, nm=imap.select(box)

		# Get the box ID from the database
		try:
			boxid=self._getBox(box)
		except InvalidBox, e:
			print "Creating " + str(e)
			boxid=self._createBox(box)

		for id in range(1, int(nm[0])+1):
			print "Fetching " + `id`
			rc, parts=imap.fetch(id, '(UID RFC822.HEADER BODY[TEXT])')
			# print parts[0][1]
			uid=None
			stuff=parts[0][0].split(' ')
			for p in range(len(stuff)):
				if stuff[p].find('UID')>=0:
					uid=stuff[p+1]
			headers=Headers(parts[0][1])
			# print "UID is " + `uid`
			# print "Body is " + parts[0][1]
			freq=self.countFrequency(uid, parts[1][1])
			# print "Headers:  " + `headers`
			try:
				self.store(boxid, headers, freq)
			except DuplicateMessage, e:
				print "Duplicate:  " + str(e)

	def __parseList(self, val):
		start=0
		for i in range(3):
			start=val.find('"', start)+1
		end=val.rfind('"')
		return(val[start:end])

	def listBoxen(self, directory='""', pattern='*'):
		"""List the IMAP boxes this thing sees."""
		rv, vals=self.imap.list(directory, pattern)
		return(map(self.__parseList, vals))

if __name__ == '__main__':
	password=getpass.getpass()
	imap=imaplib.IMAP4('mail')
	imap.login('dustin', password)

	indexer=ImapIndex(imap)

	for b in indexer.listBoxen('INBOX.archive.'):
		indexer.indexBox(b)

	for b in indexer.listBoxen('INBOX.sent-mail.'):
		indexer.indexBox(b)
