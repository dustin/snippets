#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 3F3EACF8-4B66-4FC9-A151-DB617F6669A3

import os
import sys
import glob
import commands
import unittest

class ShellScriptCase(unittest.TestCase):

    def __init__(self, dirName, fileName):
        unittest.TestCase.__init__(self)
        self.dirName=dirName
        self.fileName=fileName
        self.testScript=os.path.join(self.dirName, self.fileName) + ".test"
        self.expectedOut=os.path.join(self.dirName, self.fileName) + ".right"

    def shortDescription(self):
        return self.testScript

    def runTest(self):
        status, output = commands.getstatusoutput("sh " + self.testScript)
        self.assertEquals(status, 0)

        f=open(self.expectedOut)
        right=f.read().rstrip('\r\n')
        f.close()

        self.assertEquals(output, right)

if __name__ == '__main__':
    """Run the test suites from the given directory."""

    tests=unittest.TestSuite()

    for theDir in sys.argv[1:]:
        files=glob.glob(theDir + "/*.test")
        testFiles=[os.path.basename(x)[:-5] for x in files]

        for t in testFiles:
            tests.addTest(ShellScriptCase(theDir, t))

    testRunner = unittest.TextTestRunner(verbosity=1)
    testRunner.run(tests)
