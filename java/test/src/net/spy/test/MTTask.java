// Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
// $Id: MTTask.java,v 1.3 2002/07/12 04:43:02 dustin Exp $

package net.spy.test;

import junit.framework.Assert;

/**
 * A test that may be performed in multiple threads simultaneously.
 */
public abstract class MTTask extends Assert implements Runnable {

	private int failures=0;
	private Throwable lastFailure=null;
	private int iterations=1;
	private boolean keepGoing=true;
	private boolean stopOnFailure=true;

	/**
	 * Get an instance of MTTask.
	 *
	 * @param iterations the number of times to perform the test
	 * @param stopOnFailure if true, the test will be stopped once the
	 * 	first failure is encountered, else the test will continue to loop
	 * 	and find failures.
	 */
	public MTTask(int iterations, boolean stopOnFailure) {
		super();
		this.iterations=iterations;
		this.stopOnFailure=stopOnFailure;
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer();

		sb.append("{MTTask class=");
		sb.append(getClass().getName());
		sb.append(", failures=");
		sb.append(failures);
		if(lastFailure!=null) {
			sb.append(", last failure=");
			sb.append(lastFailure);
		}
		sb.append("}");

		return(sb.toString());
	}

	/**
	 * Return the number of failures this task has identified.
	 *
	 * @return the number of failures this task has identified
	 */
	public int getFailureCount() {
		return(failures);
	}

	/**
	 * Find out why the last failure occurred.
	 *
	 * @return the last failure, null if there has not been a failure
	 */
	public Throwable getLastFailure() {
		return(lastFailure);
	}

	/**
	 * Add a new failure, including the message.
	 *
	 * @param f add a failure, incrementing the count
	 */
	protected synchronized void addFailure(Throwable f) {
		failures++;
		lastFailure=f;

		if(stopOnFailure) {
			shutDown();
		}
	}

	/**
	 * Perform the desired test.
	 */
	public abstract void performTest();

	/**
	 * Ask the test to stop as soon as possible.  As soon as possible means
	 * after the next loop.
	 */
	public void shutDown() {
		keepGoing=false;
	}

	/**
	 * Perform any required initialization for the test.  If initialization
	 * fails, this will be marked as a failure for the test, and the test
	 * will not continue.
	 */
	protected void setUp() {
	}

	/**
	 * Perform any required uninitialization for the test.
	 */
	protected void tearDown() {
	}

	/**
	 * Run performTest() the given number of times.
	 */
	public final void run() {

		try {
			setUp();
		} catch(Throwable t) {
			t.printStackTrace();
			addFailure(t);
			shutDown();
		}

		// Run the specific number of times.
		for(int i=0; keepGoing && i<iterations; i++) {
			try {
				performTest();
			} catch(Throwable t) {
				addFailure(t);
			}
		}

		tearDown();
	}

}
