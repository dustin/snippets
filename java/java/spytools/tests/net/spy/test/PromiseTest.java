// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PromiseTest.java,v 1.1 2002/08/18 07:32:16 dustin Exp $

package net.spy.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import net.spy.util.Promise;
import net.spy.util.BrokenPromiseException;

/**
 * Test the promise implementation.
 */
public class PromiseTest extends TestCase {

	/**
	 * Get an instance of PromiseTest.
	 */
	public PromiseTest(String name) {
		super(name);
	}

	/**
	 * Get the test suite.
	 *
	 * @return this test
	 */
	public static Test suite() {
		return new TestSuite(PromiseTest.class);
	}

	/**
	 * Run this test.
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	/**
	 * Test a promise that returns an int.
	 */
	public void testIntPromise() throws BrokenPromiseException {
		Promise p=new IntPromise(17);

		Integer i1=(Integer)p.getObject();
		Integer i2=(Integer)p.getObject();
		assertNotNull(i1);
		assertNotNull(i2);

		assertEquals("First run", 17, i1.intValue());
		assertEquals("Second run", 17, i2.intValue());
	}

	//
	// Private inner classes for testing.
	//

	private class IntPromise extends Promise {

		private int myInt=-1;

		public IntPromise(int what) {
			super();
			myInt=what;
		}

		protected Object execute() throws BrokenPromiseException {
			return new Integer(myInt++);
		}

	}

}
