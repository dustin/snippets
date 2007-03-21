#!/usr/bin/env python
"""
A simple Design by Contract system for python using decorators.

Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 1EF1DAA0-001A-4841-B21E-1B15E29E59AF

import exceptions

class BreachOfContractError(exceptions.RuntimeError):
    """Base exception for any contract error."""
    pass

class PostConditionError(BreachOfContractError):
    """Exception raised when a postcondition fails."""
    def __init__(self, name):
        self.name=name

    def __repr__(self):
        return "<PostConditionError for condition %s>" % (self.name,)

    __str__ = __repr__

class PreConditionError(BreachOfContractError):
    """Exception raised when a precondition fails."""
    def __init__(self, name):
        self.name=name

    def __repr__(self):
        return "<PreConditionError for condition %s>" % (self.name,)

    __str__ = __repr__

def postCondition(name, checkFunc):
    """Postcondition check for a function call.
    
    checkFunc should look like this:

        f(rv, *a, **kwa)

    where rv is the value to be returned, *a is the list of named arguments,
    and **kwa is the dict of keyworded arguments to the wrapped function.
    """
    def wrapped(f):
        def tocall(*args, **kwargs):
            rv=f(*args, **kwargs)
            if not checkFunc(rv, *args, **kwargs):
                raise PostConditionError(name)
            return rv
        tocall.func_name=f.func_name
        tocall.__doc__=f.__doc__
        return tocall
    return wrapped

def preCondition(name, checkFunc):
    """Precondition check for a function call.
    
    checkFunc should look like this:

        f(*a, **kwa)

    where *a is the list of named arguments and **kwa is the dict of
    keyworded arguments to the wrapped function.
    """
    def wrapped(f):
        def tocall(*args, **kwargs):
            if not checkFunc(*args, **kwargs):
                raise PreConditionError(name)
            return f(*args, **kwargs)
        tocall.func_name=f.func_name
        tocall.__doc__=f.__doc__
        return tocall
    return wrapped

if __name__ == '__main__':
    import sys
    import traceback

    @postCondition('gt 3', lambda rv, *a, **kwa : rv>3)
    @postCondition('eq', lambda rv, *a, **kwa : rv == a[0])
    @preCondition('lt 9', lambda *a, **kwa : a[0] < 9)
    def someFunc(x):
        print "Returning", x
        return x

    for x in (9, 8, 7, 6, 5, 4, 3, 2):
        try:
            someFunc(int(x))
        except:
            print "Error at", x
            traceback.print_exc()
