#!/usr/bin/env python

import imaplib
import md5
import sys
import time
import getpass
import traceback

import imaprange

def boxParser(val):
    start=0
    for i in range(3):
        start=val.find('"', start)+1
    end=val.rfind('"')
    return(val[start:end])

def listBox(imap, box):
    sys.stderr.write("* Indexing %s\n" % box)
    rc, nm=imap.select(box, readonly=True)
    # print rc, nm
    msgs=[]
    for msgnum in imaprange.rangemaker(10, int(nm[0])+1):
        rc, stuff=imap.fetch(str(msgnum), '(rfc822.size envelope body.peek[])')
        for s in stuff:
            mtmp=md5.new(`s`)
            msgs.append(mtmp.hexdigest())
    msgs.sort()
    m=md5.new()
    for i in msgs:
        m.update(i)
    print box, nm, len(msgs), m.hexdigest()

if __name__ == '__main__':

    server, username=sys.argv[1], sys.argv[2]

    # Get the starting point
    startPoints=[""]
    if len(sys.argv) > 3:
        startPoints=sys.argv[3:]

    # Get the password
    password=getpass.getpass()

    # set up the connection
    imap=imaplib.IMAP4(server)
    imap.login(username, password)

    for start in startPoints:
        # Dump the boxes and get busy
        rv, boxes=imap.list('"%s"' % (start,), "*")
        # Get just the box names
        boxes=[boxParser(box) for box in boxes]

        for box in boxes:
            listBox(imap, box);
