#!/usr/bin/env python
# arch-tag: A3010F49-F919-4169-8D17-8CED7CD275E2
"""
    Order a list of nodes by dependency.

    As a commandline tool, this script takes a list of files and expects to
    find lines describing a dependency relationship between other files.

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

    def __init__(self, provides=[], requires=[]):
        """Construct a node."""
        self.provides=sets.Set(provides)
        self.requires=sets.Set(requires)

    def requiresName(self, other):
        """Return true if this DepFile requires that DepFile."""
        rv = 0
        for n in self.requires:
            if n in other.provides:
                rv = 1
        return rv

    def __repr__(self):
        return "<Node provides=%s, requires=%s>" \
            % (self.provides, self.requires)

    def __cmp__(self, other):
        assert isinstance(other, Node)
        p=list(self.provides)
        p.sort()
        op=list(other.provides)
        op.sort()
        return cmp(p, op)

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
                self.provides.add(a[1])
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
            rv=super(DepFile, self).__cmp__(other)
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
        self.nodes=[]
        self.allprovides=sets.Set()

    def add(self, node):
        """Add a new node to the dependency orderer."""
        self._checkNewNode(node)
        self.nodes.append(node)
        self.allprovides.union_update(node.provides)

    def _checkNewNode(self, node):
        """Check the validity of a new node before adding it."""
        dups=node.provides.intersection(self.allprovides)
        if len(dups) > 0:
            raise ValueError, "New node duplicates existing provides: %s" \
                % (str(dups),)

    def __order(self):
        """Perform the actual ordering."""
        output=[]
        g=copy.copy(self.nodes)
        # Provide a stable ordering
        g.sort()

        # Slight variation on a toplogical sorting algorithm.  This variation
        # uses a set to track all met requirements instead of directly checking
        # graph linkage.
        # See <http://en.wikipedia.org/wiki/Topological_sorting>

        met=sets.Set()
        q=[]
        # Iterating the original because I'm mutating the graph
        for e in self.nodes:
            if len(e.requires) == 0:
                q.append(e)
                met.union_update(e.provides)
                g.remove(e)
        # Now keep looking around until stuff is done
        while len(q) > 0:
            n=q.pop(0)
            output.append(n)
            # Add everything whose requirements are met to q
            for e in [x for x in g if x.requires.issubset(met)]:
                g.remove(e)
                met.union_update(e.provides)
                q.append(e)

        if len(g) > 0:
            raise NotPlaced(g)
        self.nodes=output

    def __checkDepend(self):
        """Verify the dependencies are all met."""
        rv=True
        saw=sets.Set()
        for f in self.nodes:
            for k in f.requires:
                if not k in saw:
                    rv=False
            saw.union_update(f.provides)
        return rv

    def getNodes(self):
        """Get the Nodes in the correct order."""
        self.__order()
        assert self.__checkDepend()
        return(self.nodes)

if __name__ == '__main__':
    do=DependencyOrderer()
    for f in sys.argv[1:]:
        df=DepFile(f)
        do.add(df)

    for f in do.getNodes():
        print f.filename
