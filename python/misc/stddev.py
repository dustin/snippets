#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 11D98AEC-28B7-11D9-A9AE-000A957659CC

def sum(a):
    rv = 0
    for i in a:
        rv += i
    return rv

def avg(a):
    return (sum(a) / len(a))

def stddev(a):
    av=float(avg(a))
    rv=0.0
    for i in a:
        rv += pow((i - av), 2)
    rv = rv / float(len(a))
    return rv


