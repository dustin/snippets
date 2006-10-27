#!/usr/bin/env python
"""

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: A7C271AB-31A5-4257-96FE-BAD5AE26F791

import sys
import sets

def doletters(w):
    rv={}
    for l in w:
        rv[l] = rv.get(l, 0) + 1
    return rv

def matches(w, letters):
    c=letters.copy()
    for l in w:
        if c.get(l, 0) > 0:
            c[l] = c[l] - 1
        else:
            return False
    return True

if __name__ == '__main__':
    letters=doletters(sys.argv[1])

    found=sets.Set()

    f=open("/usr/share/dict/words")
    for w in f:
        w=w.strip().lower()
        if len(w) > 2 and matches(w, letters):
            found.add(w)
    f.close()

    l=list(found)
    l.sort()

    for w in l:
        print w
