#!/usr/bin/env python
# arch-tag: 7D9D50F4-A5D3-4A9B-B2A9-62B1903489A0

import imaplib
import md5
import sys
import time
import getpass
import traceback

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
    rc, stuff=imap.fetch('0:' + nm[0], '(body[header.fields (message-id)])')
    ids=[]
    for s in stuff:
        # I would expect to get something for every chunk, but I get a blank
        # paren between records
        try:
            if len(s) > 1:
                headers=s[1].strip()
                name, val=headers.split(': ')
                ids.append(val)
        except ValueError, e:
            sys.stderr.write("! error on %s\n" % `s`)
            traceback.print_exc()
        except TypeError, e:
            sys.stderr.write("! error on %s\n" % `s`)
            traceback.print_exc()
    ids.sort()
    m=md5.new()
    for i in ids:
        m.update(i)
    print box, nm, len(ids), m.hexdigest()

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
