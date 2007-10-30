#!/usr/bin/env python
"""
Ev[ai]l print.

Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
"""

class Evaldict(object):

    def __init__(self, l):
        self.globals=globals()
        self.locals=l

    def __getitem__(self, k):
        return eval(k, self.globals, self.locals)

if __name__ == '__main__':
    x, y=4, 3
    print "%(x)d + %(y)d = %(x + y)d" % (Evaldict(locals()))
