#!/usr/bin/env python
"""
My GPG module.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 52E1C7DC-0810-4EB6-B254-6368D46068C0

import os
import stat
import exceptions
import subprocess
import cStringIO

class Operation(object):
    """An operation for GPG to do."""

    def getCmd(self):
        """Get the command this operation specifies."""
        raise exceptions.NotImplementedError(self.__class__.__module__ + "."
            + self.__class__.__name__ + " does not implement getCmd")

class Encrypt(Operation):
    """An encrypt operation."""

    def __init__(self, recipients):
        """Encrypt with the given list of recipients."""
        self.recipients=recipients

    def getCmd(self):
        rv=[]
        rv.append("-e")
        for i in self.recipients:
            rv.append("-r")
            rv.append(i)
        return(rv)

class AsciiArmor(Operation):
    """Use Ascii armor."""
    def getCmd(self):
        return ["-a"]

class CompositeCommand(Operation):
    """Command made up of multiple other commands."""

    def __init__(self, cmds):
        self.cmds=cmds

    def getCmd(self):
        rv=[]
        for x in self.cmds:
            rv.extend(x.getCmd())
        return rv

class GPG(object):
    """GPG Interface."""

    def __init__(self, gpg_location):
        """Get a GPG instance using the given location of the GPG binary."""

        self.gpg=gpg_location

    def __execute(self, op, instream, outstream, errstream):
        """Execute the given set of operations reading data from the given
        input stream and writing to the given output stream."""

        args=[self.gpg, '--batch']
        args.extend(op.getCmd())

        sub=subprocess.Popen(args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            close_fds=True)
        sub.stdin.write(instream.read())
        sub.stdin.close()
        outstream.write(sub.stdout.read())
        errstream.write(sub.stderr.read())

        os.waitpid(sub.pid, 0)

    def encryptString(self, recips, msg):
        """Convenience method for encrypting a message.
           Returns (encrypted_msg, warnings) as strings"""

        ops=CompositeCommand([AsciiArmor(), Encrypt(recips)])

        sbuf=cStringIO.StringIO()
        ebuf=cStringIO.StringIO()
        self.__execute(ops, cStringIO.StringIO(msg), sbuf, ebuf)
        return sbuf.getvalue(), ebuf.getvalue()

def findGPG(gpgName='gpg'):
    """Find the location of GPG on your system."""
    gpg=None
    path=os.getenv("PATH", "").split(':')
    for p in path:
        testPath=os.path.join(p, gpgName)
        if os.path.exists(testPath):
            gpg=testPath
            break
    if gpg is None:
        raise exceptions.Exception("Cannot find GPG")

    return gpg
