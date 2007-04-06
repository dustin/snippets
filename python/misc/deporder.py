#!/usr/bin/env python
# arch-tag: A3010F49-F919-4169-8D17-8CED7CD275E2
"""
    Order a list of files by dependency.

    This script takes a list of files and expects to find lines describing
    a dependency relationship between other files.

    A file may express what it provides by having a line similar to the
    following:

    @PROVIDES something

    And requirements are expressed by having one or more lines similar to
    the following:

    @REQUIRES something_else

    Everything before the @ is ignored, and only the next chunk of
    whitespace separated text is used, so this should work with any series
    of files.
"""

import sys
import copy
import sets
import string
import exceptions

class Node(object):
    """A node that may provide something, or require some things."""

    provides=None
    requires=None

    def __init__(self, provides=None, requires=[]):
        """Construct a node."""
        self.provides=provides
        self.requires=sets.Set(requires)

    def requiresName(self, other):
        """Return true if this DepFile requires that DepFile."""
        rv = 0
        for n in self.requires:
            if other.provides == n:
                rv = 1
        return rv

    def __cmp__(self, other):
        assert isinstance(other, Node)
        rv=-1
        if self.provides is None:
            if other.provides is None:
                rv=cmp(id(self), id(other))
            else:
                rv=-1
        else:
            if other.provides is None:
                rv=1
            else:
                rv=cmp(self.provides, other.provides)
        return rv

class DepFile(Node):
    """A file containing dependencies."""

    filename=None

    def __init__(self, filename):
        """Construct with a filename."""
        super(DepFile, self).__init__()
        self.filename=filename
        self.__fileInit()

    def __fileInit(self):
        f=open(self.filename)
        for line in f:
            ppos=string.find(line, '@PROVIDES')
            rpos=string.find(line, '@REQUIRES')
            if ppos >= 0:
                # Provides
                a=string.split(string.strip(line[ppos:]))
                if self.provides is not None:
                    raise "Already defined provides for " + self.filename
                self.provides = a[1]
            elif rpos >= 0:
                # Requires
                a=string.split(string.strip(line[rpos:]))
                self.requires.add(a[1])
        f.close()

    def __cmp__(self, other):
        rv=-1
        if isinstance(other, DepFile):
            rv=cmp(self.filename, other.filename)
        else:
            rv=super(DepFile, __cmp__(self, other))
        return rv

    def __repr__(self):
        return "<DepFile f=%s, provides=%s, requires=%s>" % \
            (self.filename, `self.provides`, self.requires)

class NotPlaced(exceptions.Exception):
    """Exception raised when items were not placed."""

    def __init__(self, items):
        self.deps=[]
        for i in items:
            self.deps.extend(i.requires)
        self.files=[i.filename for i in items]

    def __repr__(self):
        return "<NotPlaced, files=%s, deps=%s>" % (self.files, self.deps)

    __str__ = __repr__

class DependencyOrderer(object):
    """Deal with dependency ordering of Nodes."""

    def __init__(self):
        self.files=[]

    def add(self, df):
        """Add a new object to watch."""
        self.files.append(df)
   
    def __order(self):
        """Perform the actual ordering."""
        output=[]
        g=copy.copy(self.files)
        # Provide a stable ordering
        g.sort()

        # Slight variation on a toplogical sorting algorithm.  This variation
        # uses a set to track all met requirements instead of directly checking
        # graph linkage.
        # See <http://en.wikipedia.org/wiki/Topological_sorting>

        met=sets.Set()
        q=[]
        # Iterating the original because I'm mutating the graph
        for e in self.files:
            if len(e.requires) == 0:
                q.append(e)
                if e.provides is not None:
                    met.add(e.provides)
                g.remove(e)
        # Now keep looking around until stuff is done
        while len(q) > 0:
            n=q.pop(0)
            output.append(n)
            # Add everything whose requirements are met to q
            for e in [x for x in g if x.requires.issubset(met)]:
                g.remove(e)
                met.add(e.provides)
                q.append(e)

        if len(g) > 0:
            raise NotPlaced(g)
        self.files=output

    def __checkDepend(self):
        """Verify the dependencies are all met."""
        rv=True
        saw=sets.Set()
        for f in self.files:
            for k in f.requires:
                if not k in saw:
                    rv=False
            saw.add(f.provides)
        return rv

    def getFiles(self):
        """Get the files in the correct order."""
        self.__order()
        assert self.__checkDepend()
        return(self.files)

if __name__ == '__main__':
    do=DependencyOrderer()
    for f in sys.argv[1:]:
        df=DepFile(f)
        do.add(df)

    for f in do.getFiles():
        print f.filename
