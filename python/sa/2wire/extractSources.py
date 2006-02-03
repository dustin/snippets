#!/usr/bin/env python
# arch-tag: DB690F81-4491-44D0-A3D1-8EB8CB127E62
"""Extract all of the sources containing the given set of
serial numbers.

Usage: ./extractSources picklist sourcedir
"""

import os
import sys
import sets
import traceback

def loadPicklist(filename):
    """Load the list of serial numbers for which we want data."""
    f=open(filename)
    try:
        rv=sets.ImmutableSet([s.strip() for s in f.readlines()])
    finally:
        f.close()
    return rv

def pick(picklist, filename):
    """Display the lines whose serial numbers (first field) are in our pick
    list."""
    f=open(filename)
    # Python doesn't support a try/except/finally sort of thing, so I have to
    # separate the finally (ensure file is closed) from the exception handling
    # (one of the files has a bad line)
    try:
        # Note the exception handler is *around* the loop.  Everything after
        # the first bad record in a file will be ignored and we'll move on to
        # the next file.
        try:
            for line in f:
                serial, rest=line.split('|', 1)
                if serial in picklist:
                    # I used sys.stdout.write to preserve line endings and stuff
                    sys.stdout.write(line)
        except ValueError:
            sys.stderr.write("Found invalid line in file %s\n" % filename)
            traceback.print_exc()
            sys.stderr.write("[continuing]\n")
    finally:
        f.close()

if __name__ == '__main__':
    try:
        picklistFile, sourceDir = sys.argv[1:]
    except ValueError:
        sys.stderr.write("Usage:  %s picklist sourcedir\n" % sys.argv[0])
        sys.exit(1)

    # Load the picklist
    picklist=loadPicklist(picklistFile)

    # Walk the directory
    for root, dirs, files in os.walk(sourceDir):
        # Sort the directories so the walk is stable
        dirs.sort()
        # Sort the files, too
        files.sort()

        # Pick each file
        for filename in files:
            pick(picklist, os.path.join(root, filename))
