#!/usr/bin/env python
"""

Break up directories into managable chunks.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: chunkify.py,v 1.4 2005/04/24 04:41:26 robbie Exp $
"""

import os, sys
import zipfile
import getopt
import exceptions

class UsageError(exceptions.Exception):
    """Exception thrown for an invalid usage."""
    pass

class Output(object):

    def finished(self):
        pass

class FileOut(Output):

    def finished(self):
        self.fout.close()

class ZipOutput(FileOut):

    def __init__(self, outfn):
        self.fout=zipfile.ZipFile(outfn + ".zip", 'w')

    def addFile(self, fn):
        self.fout.write(fn)

class ListOutput(FileOut):

    def __init__(self, outfn):
        self.fout=file(outfn, 'w')

    def addFile(self, fn):
        self.fout.write(fn + "\n")

class SymlinkOutput(Output):
    def __init__(self, outfn):
        self.top=os.getcwd()
        self.mydir=outfn

    def __ensurePath(self, fn):
        if fn != "" and not os.path.exists(fn):
            self.__ensurePath(os.path.dirname(fn))
            print "Making", fn
            os.mkdir(fn)

    def addFile(self, fn):
        dn=os.path.dirname(fn)
        bfn=os.path.basename(fn)
        destdir=os.path.join(self.mydir, dn)
        destfn=os.path.join(destdir, bfn)
        # Overwrite the source file with the full path
        fn=os.path.join(self.top, fn)
        # print "Linking", fn, "->", destfn
        self.__ensurePath(destdir)
        os.symlink(fn, destfn)

class Chunker:

    # Default maximum size (650MB)
    DEFAULT_MAXSIZE=(650*1024*1024)

    def __init__(self, filebase, outClass=ListOutput, maxsize=DEFAULT_MAXSIZE):
        """Get a chunker using the given base filenames for list files.

        Optionally, supply a maximum size for each file, and choose between
        whether to make a zip file or just a list of the files."""
        # Arguments
        self.filebase=filebase
        self.outClass=outClass
        self.maxsize=maxsize

        if self.maxsize==None:
            self.maxsize=Chunker.DEFAULT_MAXSIZE

        # Accumulated size of the current file
        self.currentSize=-1
        # Current file ID (1, 2, etc...)
        self.currentFileId=0
        # New file callback
        self.nfcallback=None
        # The output object
        self.output=None
        # Now, go fetch a new file
        self.__newfile()

    def registerNewFileCallback(self, cb):
        """Register a callback function to be called each time a new file
        is required.

        The callback will be called with the Chunker instance and the new
        filename as arguments.

        Since the first file is created upon construction of an object, the
        callback may only be called beginning with the creation of the
        second file."""
        self.nfcallback=cb

    def __newfile(self):
        if self.currentSize==0:
            raise "Requested newfile with a size of 0"
        # Reset the file size to zero
        self.currentSize=0
        # If there's an open file already, close it
        if self.output:
            self.output.finished()
        # Move on to the next file.
        self.currentFileId+=1
        # Create the base filename
        fn=self.filebase + "." + str(self.currentFileId)
        # Perform the callback (if any)
        if self.nfcallback!=None:
            self.nfcallback(self, fn)
        self.output=self.outClass(fn)

    def __storeFile(self, path):
        # Get the size of the file
        size=os.path.getsize(path)
        # # print path + " is a file of " + str(size) + " bytes."
        # Figure out if we need a new file
        if self.currentSize + size > self.maxsize:
            print "Need a new file for " + path
            self.__newfile()
        # Increment the size
        self.currentSize+=size
        # Store the file.
        self.output.addFile(path)

    def process(self, path):
        """Process the given path."""
        print "Processing " + path
        if(os.path.isdir(path)):
            for f in os.listdir(path):
                self.process(os.path.join(path, f))
        else:
            try:
                self.__storeFile(path)
            except OSError, e:
                print e

def pauseCallback(chunker, fn):
    print "--- Press enter to create " + fn + " ---"
    sys.stdin.readline()

def parseSize(s):
    rv=None
    if s[-1].lower() == 'g':
        rv=float(s[:-1]) * (1024*1024*1024)
    elif s[-1].lower() == 'm':
        rv=float(s[:-1]) * (1024*1024)
    elif s[-1].lower() == 'k':
        rv=float(s[:-1]) * 1024
    else:
        rv=float(s)
    return int(rv)

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'c:m:o:p')
    except getopt.GetoptError, e:
        raise UsageError(e)

    chunkname=None
    makezip=None
    maxsize=None
    dopause=None
    outClass=ListOutput

    outClasses={'zip': ZipOutput, 'list': ListOutput, 'symlink': SymlinkOutput}

    for pair in opts:
        if pair[0]=='-c':
            chunkname=pair[1]
        elif pair[0]=='-o':
            if not outClasses.has_key(pair[1]):
                raise UsageError("Unknown output type:  %s" % pair[1])
            outClass=outClasses[pair[1]]
        elif pair[0]=='-m':
            maxsize=parseSize(pair[1])
        elif pair[0]=='-p':
            dopause=1

    if len(args)<1:
        raise UsageError("No start dir specified.")
    if chunkname==None:
        raise UsageError("No chunk name specified.")

    chunker=Chunker(chunkname, outClass, maxsize)
    if dopause:
        chunker.registerNewFileCallback(pauseCallback)
    for d in args[0:]:
        print "Working on " + d
        chunker.process(d)

def usage():
    print "Usage:  " + sys.argv[0] \
        + " -c basefile [-m chunksize] [-o list|zip|symlink] [-p] startpath"
    print "\t-c specifies the base name for each file"
    print "\t-m specifies the chunk size (in bytes, or with k/m/g extension)"
    print "\t   each file (default, 650m)"
    print "\t-o is the output type (either list, zip, or symlink)"
    print "\t-p pauses before creating each file (after the first)."
    print "\tstartpath is the directory to traverse."

if __name__ == '__main__':
    try:
        main()
    except UsageError, e:
        print e
        usage()
