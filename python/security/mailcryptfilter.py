#!/usr/bin/env python
"""

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: EE9BAFBA-CE39-42DD-A968-1383396024CE

import sys
import shutil
import rfc822

import gpg

if __name__ == '__main__':
    fromline=sys.stdin.readline()
    sys.stdout.write(fromline)

    # Parse the message
    m=rfc822.Message(sys.stdin)
    fn, fe=m.getaddr('from')
    sys.stdout.write(str(m))
    sys.stdout.write("\n")

    if 'root@' in fe:
        g=gpg.GPG(gpg.findGPG())
        recips=('primary',)
        g.encryptFile(recips, sys.stdin, sys.stdout)
    else:
        shutil.copyfileobj(sys.stdin, sys.stdout)
