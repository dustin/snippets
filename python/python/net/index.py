#!/usr/bin/env python

import imaplib
import time
import getpass
import anydbm
import pprint
import string
import stemmer

def cleanWord(w):
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
		w=ps.stem(w, 0, len(w)-1)
	return w

def countFrequency(uid, message):
	count=dict()
	for w in message.split():
		w=cleanWord(w)
		if w:
			if count.has_key(w):
				count[w]+=1
			else:
				count[w]=1
	print count

ps=stemmer.PorterStemmer()

password=getpass.getpass()

imap=imaplib.IMAP4('mail')

imap.login('dustin', password)

rc, nm=imap.select()

for id in range(1, int(nm[0])+1):
	print "Fetching " + `id`
	rc, parts=imap.fetch(id, '(UID BODY[TEXT])')
	# print parts[0][1]
	uid=None
	stuff=parts[0][0].split(' ')
	for p in range(len(stuff)):
		if stuff[p].find('UID')>=0:
			uid=stuff[p+1]
	# print "UID is " + `uid`
	# print "Body is " + parts[0][1]
	countFrequency(uid, parts[0][1])
