#!/usr/bin/env python
"""
2wire Serial number and hpid -> various password encodings.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import sys
import base64
import md5
import urllib

def unhex(s):
    """Unhexify a hex string.

    >>> unhex("44757374696e")
    'Dustin'
    """
    # Must be an even number of digits
    assert len(s) % 2 == 0
    rv=""
    for i in range(len(s)/2):
        pair=s[i*2:(i*2)+2]
        rv+=str(chr(int(pair, 16)))
    return rv

def getCWMPPassword(sn, s):
    return getHexDigestPassword(sn, s)

def getShastsaHPIDPassword(sn, s):
    return s.strip()

def getShastaAuthCodePassword(sn, s):
    return urllib.quote(s)

def getHexDigestPassword(sn, s):
    m=md5.new(unhex(s))
    m.update(sn)
    return "0010" + m.hexdigest()

if __name__ == '__main__':
    for f in (getCWMPPassword, getShastsaHPIDPassword,
        getShastaAuthCodePassword):
        print f, f(sys.argv[1], sys.argv[2])
