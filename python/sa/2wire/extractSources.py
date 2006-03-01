#!/usr/bin/env python
# arch-tag: DB690F81-4491-44D0-A3D1-8EB8CB127E62
"""Extract all of the sources containing the given set of
serial numbers.

Usage: ./extractSources -f|-t picklist sourcedir
"""

import os
import sys
import sets
import time
import stat
import tarfile
import traceback
import exceptions
import StringIO

def loadPicklist(filename):
    """Load the list of serial numbers for which we want data."""
    f=open(filename)
    try:
        rv=sets.ImmutableSet([s.strip() for s in f.readlines()])
    finally:
        f.close()
    return rv

def pick(picklist, filename, outputter):
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
                    outputter.writeLine(filename, line)
        except ValueError:
            sys.stderr.write("Found invalid line in file %s\n" % filename)
            traceback.print_exc()
            sys.stderr.write("[continuing]\n")
    finally:
        f.close()

class Outputter(object):
    """Base class for all output mechanisms."""

    def writeLine(self, ts, line):
        """Write the given line for the given timestamp."""
        raise exceptions.NotImplementedError

    def close(self):
        """Mark this thing as complete."""
        pass

class SerialOutputter(Outputter):
    """Just emit the matching lines."""

    def writeLine(self, ts, line):
        sys.stdout.write(line)

class TarOutput(Outputter):
    """Output a tar file retaining all of the base filenames"""

    def __init__(self):
        self.current=None
        self.data=[]
        self.basename=time.strftime("sourcedata_%Y%m%d%H%M%S")
        self.tarfile=tarfile.TarFile(self.basename + ".tar", "w", sys.stdout)

    def __writeCurrent(self):
        if self.data != []:
            tardata=''.join(self.data)
            sfile=StringIO.StringIO(tardata)
            filename=self.basename + "/" + os.path.basename(self.current)

            # Get a tar file that matches closely the original file, but with a
            # new name and size
            tarinf=self.tarfile.gettarinfo(self.current)
            tarinf.size=len(tardata)
            tarinf.name=filename
            # Add this to our archive
            self.tarfile.addfile(tarinf, StringIO.StringIO(tardata))

            # Clear out the currently accumulated data
            self.data=[]

    def writeLine(self, filename, line):
        # If this is a new file, close the old one and write out the entry
        if self.current is not None and self.current != filename:
            self.__writeCurrent()
        # Track the filename and the newly accumulated line
        self.current=filename
        self.data.append(line)

    def close(self):
        self.__writeCurrent()
        self.tarfile.close()

if __name__ == '__main__':
    try:
        fmt, picklistFile, sourceDir = sys.argv[1:]
    except ValueError:
        sys.stderr.write("Usage:  %s -f|-t picklist sourcedir\n" % sys.argv[0])
        sys.exit(1)

    # These are the output formats we support
    fmts={'-f': SerialOutputter(), '-t': TarOutput()}

    # Select an output format
    outputter=fmts[fmt]

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
            pick(picklist, os.path.join(root, filename), outputter)

    outputter.close()
