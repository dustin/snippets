#!/usr/bin/env python
# arch-tag: 7D9D50F4-A5D3-4A9B-B2A9-62B1903489A0

import imaplib
import sys
import time
import getpass

def boxParser(val):
    start=0
    for i in range(3):
        start=val.find('"', start)+1
    end=val.rfind('"')
    return(val[start:end])

if __name__ == '__main__':

    (server, username)=sys.argv[1:]

    password=getpass.getpass()

    imap=imaplib.IMAP4(server)

    imap.login(username, password)

    rv, boxes=imap.list('""', "*")

    print '\n'.join([boxParser(box) for box in boxes])
