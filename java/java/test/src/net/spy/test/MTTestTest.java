// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: MTTestTest.java,v 1.1 2002/07/12 07:18:27 dustin Exp $

package net.spy.test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Vector;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * This is a test to test the MTTest kit.
 */
public class MTTestTest extends MTTest {

	/**
	 * Get an instance of MTTestTest.
	 */
	public MTTestTest(String name) {
		super(name);
	}

	/**
	 * Get the test suite.
	 */
	public static Test suite() {
		return new TestSuite(MTTestTest.class);
	}

	/**
	 * Run the test suite in the text UI.
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	/**
	 * Test test.  Just an example of a regular JUnit test.
	 */
	public void testTest() {
		assertEquals("One and one aren't equal?", 1, 1);
	}

	/**
	 * Test a synchrnonized ArrayList, see if it does any better.
	 */
	public void testMTSynchronizedArrayList() {
		// This time, test an array list that's synchronized.
		List testMe=Collections.synchronizedList(new ArrayList());

		// Perform the test on the synchronized list, this should succeed
		performListTest(testMe);
	}

	/**
	 * Abuse a non-synchronized ArrayList.  Make it cry.
	 */
	public void testMTArrayListAbuse() {
		// This is the object we'll be testing, an ArrayList.  They're not
		// supposed to be thread safe.  Let's find out.
		ArrayList testMe=new ArrayList();

		// We expect this test to fail, wrap the test and catch the assertion
		boolean succeeded=false;
		try {
			performListTest(testMe);
			succeeded=true;
		} catch(AssertionFailedError afe) {
			// This is the normal case.
			succeeded=false;
		}

		if(succeeded) {
			fail("ArrayList abuse test was expected to fail, but it didn't.");
		}
	}

	private void performListTest(List testMe) {
		// Going to create all of the tests here
		Vector tasks=new Vector();

		// A total of 256 jobs working that array should be sufficient
		for(int i=0; i<256; i++) {
			// Each test will run 256 times, or until the first failure
			tasks.add(new MTTestTestTask(testMe, 256, true));
		}

		// Run with 64 at a time
		runParallel(tasks.elements(), 64);
	}

	//
	// Below this line is the private implementation of MTTask
	//

	// A globally unique number, since we can't define this in the class
	private static int globallyUniqueNumber=0;

	// This is a private inner class that contains the actual method to test.
	private class MTTestTestTask extends MTTask {

		// this reference to the ArrayList we're beating up
		private List list=null;

		// Instantiate MTTestTestTask
		public MTTestTestTask(List l, int iterations, boolean stopOnFailure) {
			super(iterations, stopOnFailure);
			this.list=l;
		} // constructor

		// For this test, we're going to add a globally unique number
		// to the array, and then see if we can find it.
		public void performTest() {
			// Get a local copy of the number as an Integer object so we
			// can store it, then look for it
			Integer mynumber=new Integer(globallyUniqueNumber++);
			// Add the number to the list
			list.add(mynumber);
			// Verify it's there.
			assertTrue("List did not contain " + mynumber,
				list.contains(mynumber));
		} // performTest

	} // class MTTestTestTask

}
