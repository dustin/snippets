#!/usr/bin/env python

import imaplib
import time
import getpass
import anydbm
import pprint

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
	print "UID is " + `uid`
	print "Body is " + parts[0][1]
