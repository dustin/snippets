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

# XXX:  http://en.wikipedia.org/wiki/Topological_sorting

import sys
import copy
import string
import exceptions

class DepFile:
    """A file containing dependencies."""

    def __init__(self, filename):
        """Construct with a filename."""
        self.filename=filename

        self.provides=None
        self.requires=[]

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
                self.requires.append(a[1])
        f.close()

    def __repr__(self):
        return "<DepFile provides=" + self.provides + ">"

    def getRequirements(self):
        """Get a list of the named requirements for this DepFile."""
        return self.requires

    def requiresName(self, other):
        """Return true if this DepFile requires that DepFile."""
        rv = 0
        for n in self.requires:
            if other.getProvides() == n:
                rv = 1
        return rv

    def getProvides(self):
        """Get the name of the thing this DepFile provides."""
        return self.provides

class NotPlaced(exceptions.Exception):
    """Exception raised when items were not placed."""

    def __init__(self, items):
        self.deps=[]
        for i in items:
            self.deps.extend(i.getRequirements())

    def __repr__(self):
        return "<NotPlaced, deps=" + `self.deps` + ">"

    __str__ = __repr__

class DependencyOrderer:
    """Deal with dependency ordering of DepFiles."""

    def __init__(self):
        self.files=[]

    def add(self, df):
        """Add a new object to watch."""
        self.files.append(df)

    def __order(self):
        """Perform the actual ordering."""
        output=[]
        notplaced=copy.copy(self.files)
        # Provide a stable ordering
        notplaced.sort(lambda a, b: cmp(a.filename, b.filename))

        runs = 0
        while len(notplaced)>0:
            if runs > len(self.files):
                raise NotPlaced(notplaced)
            runs = runs + 1

            # Track the ones that were just added to preserve order
            justadded=[]

            for d in notplaced:
                missing=0
                for r in d.getRequirements():
                    found = 0
                    for o in output:
                        if o.getProvides() == r:
                            if not o in justadded:
                                found = 1
                    if found == 0:
                        missing = missing + 1
                if missing == 0:
                    notplaced.remove(d)
                    output.append(d)
                    justadded.append(d)
        self.files=output

    def __checkDepend(self):
         """Verify the dependencies are all met."""
         saw={}
         for thing in self.files:
            for k in thing.getRequirements():
                if not saw.has_key(k):
                    raise "Dependency not met:  " + k + " for " + `thing`
            saw[thing.getProvides()]=1

    def getFiles(self):
        """Get the files in the correct order."""
        self.__order()
        self.__checkDepend()
        return(self.files)

if __name__ == '__main__':
    do=DependencyOrderer()
    for f in sys.argv[1:]:
        df=DepFile(f)
        do.add(df)

    for f in do.getFiles():
        print f.filename
