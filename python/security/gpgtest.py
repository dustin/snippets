#!/usr/bin/env python
"""

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""

import os
import unittest
import exceptions

import gpg

class OpTest(unittest.TestCase):

    def testBase(self):
        """Test the operation base."""
        op=gpg.Operation()
        try:
            rv=op.getCmd()
            self.fail("Shouldn't be able to invoke op.getCmd, got " + rv)
        except exceptions.NotImplementedError, e:
            self.assertEquals(str(e), "gpg.Operation does not implement getCmd")

        class Tmp(gpg.Operation):
            pass
        t=Tmp()
        try:
            rv=t.getCmd()
            self.fail("Shouldn't be able to invoke t.getCmd, got " + rv)
        except exceptions.NotImplementedError, e:
            self.assertEquals(str(e), "__main__.Tmp does not implement getCmd")

    def testEncrypt(self):
        """Test the encrypt operation"""
        op=gpg.Encrypt(['home', 'work', 'other'])

        rv=op.getCmd()
        self.assertEquals(rv, ['-e', '-r', 'home', '-r', 'work', '-r', 'other'])

    def testAsciiArmor(self):
        """Test the ascii armor operation."""
        op=gpg.AsciiArmor()
        self.assertEquals(op.getCmd(), ['-a'])

    def testKeyringCmd(self):
        """Test the keyring command."""
        op=gpg.Keyring("/tmp/whatever")
        self.assertEquals(op.getCmd(), ['--keyring', '/tmp/whatever'])

    def testSecretKeyringCmd(self):
        """Test the secret keyring command."""
        op=gpg.SecretKeyring("/tmp/whatever")
        self.assertEquals(op.getCmd(), ['--secret-keyring', '/tmp/whatever'])

    def testNoDefaultKeyringCmd(self):
        """Test the command that disables default keyrings."""
        op=gpg.NoDefaultKeyrings()
        self.assertEquals(op.getCmd(), ['--no-default-keyring'])

    def testCompositeCommand(self):
        """Test the command compositing."""
        op=gpg.CompositeCommand([gpg.AsciiArmor(),
            gpg.Encrypt(["test", "test2"])])
        self.assertEquals(op.getCmd(),
            ['-a', '-e', '-r', 'test', '-r', 'test2'])

class GPGTest(unittest.TestCase):
    """Test the GPG object."""

    def testFindGPG(self):
        """Positive test finding gpg"""
        # Just make sure we got something
        self.failIf(len(gpg.findGPG()) == 0)

    def testNegativeFindGPG(self):
        """Negative test finding gpg."""
        # This should fail to find the requested binary
        try:
            fnd=gpg.findGPG('ThisShouldNotExist')
            self.fail("Shouldn't have found ThisShouldNotExist, got:  " + fnd)
        except exceptions.Exception, e:
            self.assertEquals(str(e), "Cannot find GPG")

    def assertEncrypted(self, data):
        """This is a little silly in that it really just checks to see that
        something *looks* encrypted, but it's a start."""
        lines=data.split("\n")
        self.failUnless("-----BEGIN PGP MESSAGE-----" in lines)
        self.failUnless("-----END PGP MESSAGE-----" in lines)
        # begin, end, version, empty line, data, checksum
        self.failUnless(len(lines) > 6)

    def testEncryptString(self):
        """Test encrypting a string."""
        g=gpg.GPG(gpg.findGPG())
        encrypted, warnings=g.encryptString(['primary'], 'hello')
        self.assertEncrypted(encrypted)

    def testEncryptWithKeyring(self):
        """Test encrypting with a specific keyring."""
        # Where our keyrings are
        pubring=os.path.join(os.getcwd(), "rings", "pubtest.gpg")
        g=gpg.GPG(gpg.findGPG())
        encrypted, warnings=g.encryptString(['nobody@example.com'], 'hello',
            pubring)
        self.assertEncrypted(encrypted)

    def testFailingEncryption(self):
        """Test an attempt to encrypt with an invalid key should fail."""
        g=gpg.GPG(gpg.findGPG())
        try:
            encrypted, warnings=g.encryptString(['ThisKeyDoesNotExist'],
                'hello')
            fail("Expected encryption to fail, returned\n" + encrypted
                + " with warnings:\n" + warnings)
        except gpg.EncryptionError, e:
            self.failUnless(str(e).find("public key not found") > 0,
                "Got the wrong error:\n" + str(e))

if __name__ == '__main__':
    unittest.main()
