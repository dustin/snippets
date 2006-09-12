#!/usr/bin/env python2.4
"""
Example decorators and usage.

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 8D68FCF6-D5A0-48B5-9105-438B87D4D9D5

import os
import sys

def perfometerWithName(name):
    """Example decorator that takes an argument."""
    def wrapped(f):
        def tocall(*args, **kwargs):
            stimes=os.times()
            try:
                return f(*args, **kwargs)
            finally:
                etimes=os.times()
                print >>sys.stdout, "** Executed %s (%s) in %.2fu %.2fs %.2fr" \
                    % (f, name, etimes[2] - stimes[2], etimes[3]-stimes[3],
                        etimes[4]-stimes[4])
        # Bring up the original function name and documentation
        tocall.func_name = f.func_name
        tocall.__doc__ = f.__doc__
        return tocall
    return wrapped

def perfometerNoName(f):
    """Example decorator that doesn't take an argument."""
    def tocall(*args, **kwargs):
        stimes=os.times()
        try:
            return f(*args, **kwargs)
        finally:
            etimes=os.times()
            print >>sys.stdout, "** Executed %s in %.2fu %.2fs %.2fr" \
                % (f, etimes[2] - stimes[2], etimes[3]-stimes[3],
                    etimes[4]-stimes[4])
    # Bring up the original function name and documentation
    tocall.func_name = f.func_name
    tocall.__doc__ = f.__doc__
    return tocall

@perfometerWithName('forilla')
def testMeWN(msg):
    """Example function decorated with an argument."""
    print msg

@perfometerNoName
def testMeNN(msg):
    """Example function decorated without an argument."""
    print msg

if __name__ == '__main__':
    testMeWN("hello")
    testMeNN("hello")
