
// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: DigestTest.java,v 1.1 2002/08/18 07:32:16 dustin Exp $

package net.spy.test;

import java.util.HashSet;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import net.spy.util.Digest;
import net.spy.util.PwGen;

/**
 * Test the digest imlementation and password generator.
 */
public class DigestTest extends TestCase {

	/**
	 * Get an instance of DigestTest.
	 */
	public DigestTest(String name) {
		super(name);
	}

	/**
	 * Get the test suite.
	 *
	 * @return this test
	 */
	public static Test suite() {
		return new TestSuite(DigestTest.class);
	}

	/**
	 * Run this test.
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	/**
	 * A basic test of the password generator.  Ensure the password
	 * generator won't generate the same password if called several times.
	 */
	public void testPasswordGenerator() {
		HashSet words=new HashSet();
		PwGen gen=new PwGen();

		for(int i=0; i<1000; i++) {
			String pw=gen.getPass(8);
			assertTrue("Generated a duplicate password on attempt " + i,
				(!words.contains(pw)));
			words.add(pw);
		}
	}

	/**
	 * Test the password hashing.  Do a couple rounds of passwords and make
	 * sure the hashing consistently works.
	 */
	public void testPasswordHash() {
		PwGen gen=new PwGen();
		Digest d=new Digest();

		for(int i=0; i<10; i++) {
			String pw=gen.getPass(8);
			String hpw=d.getHash(pw);
			assertTrue("Password checking failed", d.checkPassword(pw, hpw));
		}
	}

}
