#!/usr/bin/env python
"""
A pipe interface to rrdtool.

Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
$Id: rrdpipe.py,v 1.5 2002/03/28 23:12:17 dustin Exp $
"""

import os
import sys
import time
import select
import pprint
import exceptions

class PipeFile:
    """Make a file object out of the ends of a pipe."""

    def __init__(self, readend, writeend):
        """Get a PipeFile from a read and write end of a pipe respectively."""
        self.readend=readend
        self.writeend=writeend
        self.isopen=1
        self.buffer=''

    def readline(self):
        """Read a line from the pipe file."""
        # XXX  This is really inefficient.  When that becomes a problem,
        # XXX  I'll implement buffering.
        rv=''
        c=self.read(1)
        while len(c) == 1 and c in '\r\n':
            c=self.read(1)
        while len(c)==1 and c not in '\r\n':
            rv+=c
            c=self.read(1)
        return rv

    # Find out of there's data available.
    def __readReady(self):
        rlist, wlist, xlist = select.select([self.readend], [], [], .01)
        return(self.readend in rlist)

    def read(self, size):
        """Read a block from the pipe."""
        return os.read(self.readend, size)

    def write(self, s):
        """Write a string to the pipe."""
        rv=os.write(self.writeend, s)
        return(rv)

    def getReader(self):
        """Get the file descriptor of the reader end of the pipe."""
        return(self.readend)

    def getWriter(self):
        """Get the file descriptor of the writer end of the pipe."""
        return(self.writeend)

    def close(self):
        """Close both descriptors in this pipe if it's not been closed
        already."""
        if self.isopen:
            os.close(self.readend)
            os.close(self.writeend)
            self.isopen=None

    def __del__(self):
        self.close()

class RRDError(exceptions.Exception):
    """Exception raised when an rrd error is detected."""
    pass

class SubprocessBroken(exceptions.Exception):
    """Exception raised when an the subprocess isn't functioning."""
    pass

class RRDPipe:
    """A pipe interface to rrdtool."""

    def __init__(self, rrdtool='rrdtool'):
        """Get an rrdtool pipe."""

        r1=None
        r2=None
        w1=None
        w2=None
        self.pid=0
        self.pfile=None

        try:
            r1, w1=os.pipe()
            r2, w2=os.pipe()
            self.pfile=PipeFile(r1, w2)
            # Fork off rrdtool
            self.pid = os.fork()
            if self.pid == 0:
                self.pfile.close()
                os.dup2(r2, 0)
                os.dup2(w1, 1)
                os.dup2(w1, 2)
                os.execv(rrdtool, [rrdtool, '-'])
                os._exit(1)
        except:
            if r1: os.close(r1)
            if r2: os.close(r2)
            if w1: os.close(w1)
            if w2: os.close(w2)
            # Rethrow
            raise sys.exc_info()[0], sys.exc_info()[1], sys.exc_info()[2]

    def __checkProcess(self):
        p, e=os.waitpid(self.pid, os.WNOHANG)
        if p==self.pid:
            self.pid=0
            msg=''
            if os.WIFSIGNALED(e):
                msg="Exited with signal " + str(os.WTERMSIG(e))
            else:
                msg="Exit code:  " + str(os.WEXITSTATUS(e))
            raise SubprocessBroken(msg)

    def sendCommand(self, cmd):
        """Send an arbitrary command to the rrdtool backend."""
        self.__checkProcess()
        # print "Sending command:  " + cmd
        self.pfile.write(cmd.rstrip() + "\n")
        rv=list()
        line=self.pfile.readline()
        while line[0:2] != 'OK':
            rv.append(line.rstrip())
            line=self.pfile.readline()
        if len(rv)>0 and rv[0][0:5] == 'ERROR':
            raise RRDError(rv[0])
        return rv

    def last(self, rrdfile):
        """Get the timestamp of the last record inserted into a given rrd
        file."""
        return int(self.sendCommand("last " + rrdfile)[0])

    def fetch(self, rrdfile, cf='AVERAGE', res=None, start=None, end=None):
        """Dump the data from the given rrdfile.

        The data will be returned as a tuple where each entry in the tuple
        is another tuple which contains an integer timestamp and a tuple of
        float columns from the rrd.  *whew*

        Columns where no data was available will contain ``None''

        fetch('file.rrd') =>
           ((1014768000, (1, 2, 3, 4))
            (1014854400, (1, 2, 3, None)))
        """

        # Build out the commandline
        cmd='fetch ' + rrdfile + ' ' + cf
        if res != None:
            cmd+=' -r ' + str(res)
        if start != None:
            cmd+=' -s ' + str(start)
        if end != None:
            cmd+=' -e ' + str(end)

        # Run the command, skip the first two lines
        stuff=self.sendCommand(cmd)[2:]
        rv=[]
        for row in stuff:
            t, rest=row.split(':')
            t=int(t)
            rows=[]
            for p in rest.split():
                if p == 'NaN':
                    p=None
                else:
                    p=float(p)
                rows.append(p)
            rv.append((t, tuple(rows)))
        return tuple(rv)

    def __del__(self):
        """Clean up after the pipe."""
        if self.pfile:
            # print "Closing pipe file."
            self.pfile.close()
        if self.pid > 0:
            # print "Killing child " + str(self.pid)
            try:
                os.kill(self.pid, 15)
            except exceptions.OSError, e:
                # No such process is OK
                if e[0] != 3:
                    raise e
            # print "Waiting for child."
            pid, status=os.waitpid(self.pid, 0)

if __name__ == '__main__':
    rp=RRDPipe('/usr/local/rrdtool-1.0.33/bin/rrdtool')
    # rp=RRDPipe()

    print rp.last(sys.argv[1])
    starttime=(int(time.time())-(86400*30))
    pprint.pprint(rp.fetch(sys.argv[1], start=starttime))
