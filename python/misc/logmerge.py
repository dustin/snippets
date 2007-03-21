#!/usr/bin/env python

import gzip
import time
import string
import sys
import os
# In-place sorting
import bisect

class LogFile:
    """Represents an individual log file."""

    # Translation for common log format timestamp parsing
    clftstranslation=string.maketrans(":/", "  ")

    def __init__(self, filename):
        """Initialize a new logfile object."""

        self.__filename=filename
        self.__isOpen=None
        # Open the file to read the initial data
        self.__open()
        # Close it again so we don't have the FD sitting open
        self.__close()

    def __close(self):
        sys.stderr.write("# Closing " + `self` + "\n")
        self.__input.close()
        self.__isOpen=None

    def __open(self):
        sys.stderr.write("# Opening " + `self` + "\n")
        if self.__filename[-3:] == ".gz":
            self.__input=gzip.open(self.__filename)
        else:
            self.__input=open(self.__filename)
        # Mark it as open
        self.__isOpen=1
        # Get the next line
        self.next()

    def next(self):
        """Get the next line in this file."""

        # Make sure the file's open
        if self.__isOpen == None:
            self.__open()

        self.__currentLine=self.__input.readline()
        if self.__currentLine == '':
            self.__timestamp=None
            self.__close()
        else:
            self.__timestamp=self.__getTimestamp()

    def getLine(self):
        """Get the current line in this file."""
        return(self.__currentLine)
    
    def getTimestamp(self):
        """Get the current timestamp."""
        return(self.__timestamp)

    def __cmp__(self, other):
        """Compare two objects by timestamp."""
        return(cmp(self.getTimestamp(), other.getTimestamp()))
    
    def __getTimestamp(self):
        """Parse the timestamp"""

        try:
            rv=self.__oldGetTimestamp()
        except ValueError:
            rv=self.__clfGetTimestamp()
        return rv

    def __clfGetTimestamp(self):
        """Parse a timestamp in Common Log Format."""

        months={'Jan':1, 'Feb':2, 'Mar':3, 'Apr':4, 'May':5, 'Jun':6,
            'Jul':7, 'Aug':8, 'Sep':9, 'Oct':10, 'Nov':11, 'Dec':12}

        s=self.__currentLine
        ts=s[string.index(s,"[")+1:string.index(s,"]")]

        ts=string.split(string.translate(ts, self.clftstranslation))

        rv=time.mktime(int(ts[2]), months[ts[1]], int(ts[0]),
            int(ts[3]), int(ts[4]), int(ts[5]), 0, 0, -1)

        return(rv)

    def __oldGetTimestamp(self):
        """Parse the timestamp in the old format."""
        stuff=string.split(self.__currentLine, " ")
        d,t=string.split(stuff[0], 'T')
        y,m,d=string.split(d,'-')
        H,M,S=string.split(t,':')
        return(time.mktime((int(y),int(m),int(d),int(H),int(M),int(S),0,0,-1)))

    def __repr__(self):
        """Print me"""

        return( "<LogFile " + self.__filename + ">")

class LogMux:
    """Watch a bunch of LogFile objects and return the records in
    chronological order."""

    def __init__(self):
        """Initialize."""

        self.__logfiles=[]

    def addLogFile(self, lf):
        """Add a new logfile to the mux."""
        # Keep it in sequence
        bisect.insort(self.__logfiles, lf)

    def getQueue(self):
        """Get the queue."""
        return(self.__logfiles)
    
    def next(self):
        """Get the next log entry"""

        rv=''

        if len(self.__logfiles) > 0:
            # Remove it from the list
            old=self.__logfiles[0]
            del(self.__logfiles[0])
            # Get the value
            rv=old.getLine()
            # Seek forward
            old.next()
            # Add it back (sorted) if there's more data
            if old.getLine() != '':
                bisect.insort(self.__logfiles, old)
            else:
                sys.stderr.write("### finished " + repr(old) + "\n")

        return rv

if __name__ == '__main__':

    if sys.argv[1] == '-':
        outfile=sys.stdout
    else:
        if os.path.exists(sys.argv[1]):
            raise sys.argv[1] + " already exists."
        outfile=gzip.GzipFile(sys.argv[1], 'w', 3)

    lm=LogMux()
    for fn in sys.argv[2:]:
        lm.addLogFile(LogFile(fn))
    # sys.stderr.write("# Queue:\n")
    # for lf in lm.getQueue():
        # sys.stderr.write("#  " + `lf` + ":" + `lf.getTimestamp()` +  "\n")
    
    line=lm.next()
    while line!='':
        outfile.write(line)
        line=lm.next()
    
    outfile.close()
