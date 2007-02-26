#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: DD384AFF-D5AB-4C17-A5A6-11F70AB6C6CF

# XXX:   Note, this doesn't work.  Still trying to figure out the protocol.

import sys
import shutil
import rfc822
import smtplib
import cStringIO

import gpg

PORT=10026

def deliver(fromaddr, toaddr, headers, bodystream):
    c=smtplib.SMTP(host='127.0.0.1', port=PORT)
    c.ehlo(name='localhost')
    c.mail(fromaddr)
    c.rcpt(toaddr)
    c.data(str(headers) + "\n" + bodystream.read())
    c.quit()

if __name__ == '__main__':
    # Parse the message
    m=rfc822.Message(sys.stdin)
    fn, fe=m.getaddr('from')
    tn, te=m.getaddr('to')

    if 'root@' in fe:
        g=gpg.GPG(gpg.findGPG())
        recips=('primary',)
        f=cStringIO.StringIO()
        g.encryptFile(recips, sys.stdin, f)
        f.seek(0)
        deliver(fe, te, m, f)
    else:
        deliver(fe, te, m, sys.stdin)
