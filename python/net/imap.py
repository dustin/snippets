#!/usr/bin/env python

import imaplib
import time
import getpass

password=getpass.getpass()

imap=imaplib.IMAP4('mail')

imap.login('dustin', password)

imap.select()

# print imap.fetch(1, '(UID FLAGS)')
# print imap.store(1, '+FLAGS', 'DustinHack')
# (status, flags) = imap.fetch(1, '(UID FLAGS)')
# print flags

# (status, messages) = imap.search('', 'FLAGGED')
# (status, messages) = imap.search('', 'KEYWORD', 'DustinHack')

# Figure out how far back we're going to search.
thetime = time.time() - (30*86400)
imaptime=imaplib.Time2Internaldate(thetime).split(' ')[1]

(status, messages) = imap.search('', 'BEFORE', imaptime)
if len(messages[0]) > 0:
    messages=messages[0].strip().split(' ')
    for id in messages:
        (status, res)=imap.fetch(id, 'FLAGS')
        res=res[0]
        print "Message " + id + ":  ", imaplib.ParseFlags(res)
        #imap.store(id, '+FLAGS', '\\DELETED')
        #imap.store(id, '-FLAGS', 'DustinHack')
