#!/usr/bin/env python
"""

Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
$Id: etherealMuxTrace.py,v 1.3 2003/04/17 01:20:08 dustin Exp $
"""

import sys
import array
import exceptions

class EtherealStream:
    """Stream of data from ethereal hex dumps"""

    def __init__(self, f):
        """Initialize with a file stream"""
        self.f=f
        # Initialize the first line
        self.line = 'X'
        self.nextLine()

    def currentLine(self):
        """Get the current line of data"""
        return self.line

    def __decodeVal(self, val):
        return int(val, 16)

    def nextBlock(self):
        """Get the next block of data.
            The return value is a tuple.  The first element in the tuple is
            a number indicating the column from which the block came.  The
            second value is a tuple of integers indicating the results."""
        l=self.nextLine()
        part=0
        data=l[10:58].strip()
        # If it's not in the first column, look in the second column
        if(data == ''):
            part=1
            data=l[88:136].strip()
        # print "# Part " + `part` + ":  " + data
        drv=tuple(map(self.__decodeVal, data.split()))
        return (part, drv)


    def nextLine(self):
        """Get the next line."""
        lastLine = self.line
        if lastLine == '':
            raise exceptions.EOFError
        self.line = f.readline()
        return lastLine

    def hasMoreData(self):
        """True if there is more data to process"""
        return self.line != ''

class MuxCommand:
    """Superclass for mux commands"""

    def __init__(self, a):
        """Initialize the command"""
        # The parts from a
        self.parts=a
        # How much we've used
        self.used=0
        # The stuff that's significant in stringing
        self.extra=()

    def nextByte(self):
        """Get the next byte from the stream."""
        rv=self.parts[self.used]
        self.used=self.used+1
        return rv

    # Alias
    readByte = nextByte

    def readWord(self):
        """Read an int value from the stream."""
        rv = self.nextByte()
        rv = rv << 8
        rv |= self.nextByte()
        return rv

    def readString(self):
        """Read a string value from the stream."""
        l = self.readWord()
        start = self.used
        end = start + l
        data = self.parts[start:end]
        self.used = end
        return (array.array('B', list(data)).tostring())

    def getUnused(self):
        """Get the portion of the data that was not used."""
        return self.parts[self.used:]

    def __repr__(self):
        """String me"""
        cparts=str(self.__class__).split('.')
        return "[" + cparts[-1] + " " + `self.extra` + "]"

    def __str__(self):
        """String me"""
        cparts=str(self.__class__).split('.')
        rv = "[" + cparts[-1] + " ["

        for s in self.extra:
            rv += str(s) + ", "

        rv += "]]"
        return rv

    def xmlsafe(self, s):
        rv=""
        for c in s:
            # It'd be nice to get it to do something cooler in here
            if c == '<':
                rv += "<ul>&lt;"
            elif c == '>':
                rv += "&gt;</ul>"
            elif c == "&":
                rv += "&amp;"
            else:
                rv += c
        return rv

    def toHTML(self):
        """HTML me"""
        cparts=str(self.__class__).split('.')
        rv="<b>" + cparts[-1] + "</b><br><ul>"
        for s in self.extra:
            rv+="<li>" + self.xmlsafe(str(s)) + "</li>"
        rv + "</ul>"
        return rv

# Commands with no args

class ShutdownCommand(MuxCommand):
    pass

class YieldCommand(MuxCommand):
    pass

class UpCommand(MuxCommand):
    pass

# Command that just take a channel

class BasicChannelCommand(MuxCommand):
    """Basic channel commands"""

    def __init__(self, a):
        MuxCommand.__init__(self, a)
        self.channel = self.readWord()
        self.extra = (self.channel, )

class OpenCommand(BasicChannelCommand):
    pass

class OpenAcceptCommand(BasicChannelCommand):
    pass

class ChannelSelectCommand(BasicChannelCommand):
    pass

class ChannelShutdownCommand(BasicChannelCommand):
    pass

# Commands that take a string

class BasicDataCommand(MuxCommand):
    """Basic data (string) commands"""

    def __init__(self, a):
        MuxCommand.__init__(self, a)
        self.data = self.readString()
        self.extra = (self.data, )

class DataCommand(BasicDataCommand):
    pass
class IdentifyCommand(BasicDataCommand):
    pass
class NonceCommand(BasicDataCommand):
    pass
class AuthResponse(BasicDataCommand):
    pass

class ErrorCommand(MuxCommand):
    """Error is kinda special"""
    def __init__(self, a):
        MuxCommand.__init__(self, a)
        self.cmd = chr(self.readByte())
        self.channel = self.readWord()
        self.code = self.readWord()
        self.msg = self.readString()
        self.extra = (self.cmd, self.channel, self.code, self.msg )

# Map a character to its type
typeMap={
        'O': OpenCommand,
        'A': OpenAcceptCommand,
        'C': ChannelSelectCommand,
        'Q': ChannelShutdownCommand,
        'E': ErrorCommand,
        'Z': ShutdownCommand,
        'D': DataCommand,
        'Y': YieldCommand,
        'I': IdentifyCommand,
        'N': NonceCommand,
        'P': AuthResponse,
        'U': UpCommand
    }

def commandFactory(tuple):
    """Take a tuple of bytes and return a tuple containing the command that
        was created and the portions of the tuple that were not used for the
        construction of that tuple."""

    # Look up the command type from the type map and instantiate it
    # ``You are not expected to understand this.''
    cmd = typeMap[chr(tuple[0])](tuple[1:])
    return (cmd, cmd.getUnused())

class MuxPrinter:
    """Take data data EtherealStream and produce a mux trace."""

    def __init__(self, names):
        """Init with a tuple of names of the streams."""

        if len(names) < 2:
            raise "Need at least two names for a MuxPrinter"
        self.names=names
        # Get the initial levels
        self.levels=map(lambda x: 1, names)
        self.current=0
        self.data=()

    def addData(self, t):
        """Add a tuple of data from the stream.
           If this chunk represents a different stream (client/server),
           then the accumulated data will be printed."""
        id, data = t
        if id != self.current:
            self.printData()
            self.data=data
            self.current = id
        else:
            self.data = self.data + data

    def printData(self):
        """Print the current data."""
        a=self.data
        # Get the commands
        while len(a) > 0:
            cmd, a = commandFactory(a)
            # Starting HTML
            if self.current == 0:
                print "<tr><td>"
            else:
                print "<tr><td>&nbsp;</td><td>"
            print """<div class="level""" + `self.levels[self.current]` \
                + """">"""
            # Print the command
            print cmd.toHTML() + "<br>\n"
            # Check for level changes
            if isinstance(cmd, YieldCommand):
                self.levels[self.current]=1
            elif isinstance(cmd, ChannelSelectCommand):
                self.levels[self.current]=2
            # Ending HTML
            print "</div>"
            if self.current == 0:
                print "</td><td>&nbsp;</td></tr>"
            else:
                print "</td></tr>"

if __name__ == '__main__':
    f=open(sys.argv[1])
    e=EtherealStream(f)
    p=MuxPrinter( ('client', 'server'))

    print """<html>
        <head>
            <title>Mux Trace</title>
            <link rel="stylesheet" type="text/css"
                href="http://bleu.west.spy.net/~dustin/tmp/mux.css">
        </head>

        <body>
        <table border="1" width="100%">
        <tr>
            <th>Client</th>
            <th>Server</th>
        </tr>
        """

    while e.hasMoreData():
        p.addData(e.nextBlock())
    p.printData()

    print """</table></body></html>"""
