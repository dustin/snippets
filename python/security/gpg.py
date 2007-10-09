#!/usr/bin/env python
"""
My GPG module.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

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

class NoDefaultKeyrings(Operation):
    """Do not use the default keyrings."""
    def getCmd(self):
        return ['--no-default-keyring']

class Keyring(Operation):
    """Add a keyring location."""

    krOp='--keyring'

    def __init__(self, loc):
        """Initialize a Keyring object with the given location."""
        self.loc=loc

    def getCmd(self):
        return [self.krOp, self.loc]

class SecretKeyring(Keyring):
    """Add a secret keyring location."""

    krOp='--secret-keyring'

class CompositeCommand(Operation):
    """Command made up of multiple other commands."""

    def __init__(self, cmds):
        self.cmds=cmds

    def getCmd(self):
        rv=[]
        for x in self.cmds:
            rv.extend(x.getCmd())
        return rv

class EncryptionError(exceptions.Exception):
    """Exception thrown when there are problems encrypting data."""

class GPG(object):
    """GPG Interface."""

    def __init__(self, gpg_location):
        """Get a GPG instance using the given location of the GPG binary."""

        self.gpg=gpg_location

    def __execute(self, op, instream, outstream):
        """Execute the given set of operations reading data from the given
        input stream and writing to the given output stream.
        Returns the string containing all warnings."""

        args=[self.gpg, '--batch']
        args.extend(op.getCmd())

        errs=None

        sub=subprocess.Popen(args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            close_fds=True)

        try:
            try:
                sub.stdin.write(instream.read())
                sub.stdin.close()

                errs=sub.stderr.read()
                outstream.write(sub.stdout.read())
            except IOError, e:
                errs=sub.stderr.read()
        finally:
            pid, status=os.waitpid(sub.pid, 0)
            if status != 0:
                raise EncryptionError(errs)

        return errs

    def encryptString(self, recips, msg, pubring=None):
        """Convenience method for encrypting a message.
           Returns (encryptedString, warningMessages) as strings"""

        keyringOps=[]
        if pubring is not None:
            keyringOps=[NoDefaultKeyrings(), Keyring(pubring) ]

        ops=CompositeCommand(keyringOps + [AsciiArmor(), Encrypt(recips)])

        sbuf=cStringIO.StringIO()
        warnings=self.__execute(ops, cStringIO.StringIO(msg), sbuf)
        return sbuf.getvalue(), warnings

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
