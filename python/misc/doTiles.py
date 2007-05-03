#!/usr/bin/env python
"""
Brandon's tile distribution problem.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

import sys
import random

def checkVLineup(older, newer):
    """Check vertical matches between two rows."""
    rv=True
    assert len(older) == len(newer)
    for i in range(len(older)):
        if older[i] == newer[i]:
            rv=False
    return rv

def wouldBeValid(rows):
    """Return true if these rows would be considered valid."""
    rv=True
    # Check horizontal
    assert 'c' == 'c'
    l=rows[-1]
    for i in range(len(l)-1):
        if l[i] == l[i+1]:
            rv=False

    # Check every other row.  Allow a b a, but not a b a b
    for i in range(len(l)-3):
        if l[i] == l[i+2] and l[i+1] == l[i+3]:
            rv=False

    # Check vertical
    if len(rows) > 1:
        # Check the row immediately above
        prev, cur=rows[-2], rows[-1]
        if not checkVLineup(prev, cur):
            rv=False

        # Go back another row to avoid vertical ``aba'' patterns.
        if len(rows) > 2:
            if not checkVLineup(rows[-3], rows[-1]):
                rv=False

        # Check the diagonal (three million dead)
        for i in range(len(prev)-1):
            if prev[i-1] == cur[i] or cur[i-1] == prev[i] \
                or prev[i+1] == cur[i] or cur[i+1] == prev[i]:
                rv=False
    return rv

def makeRow(r, tiles, w):
    rv=[]
    while len(rv) < w:
        # Make sure there's no duplicate in the row (don't make something we
        # know is wrong)
        t=r.choice(tiles)
        rv.append(t)
    return rv

def makeWall(tiles, w, h):
    """Make a wall with the given sequence of tiles."""
    r=random.Random()
    rv=[]
    failures=0
    for i in range(h):
        l=makeRow(r, tiles, w)
        while not wouldBeValid(rv + [l]):
            failures+=1
            l=makeRow(r, tiles, w)
        print "Accepted", l
        rv.append(l)
    return rv, failures

if __name__ == '__main__':
    wall, failures=makeWall(sys.argv[1], int(sys.argv[2]), int(sys.argv[3]))
    for h in wall:
        print h
    print "Had %d failures" % failures
