// Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
// $Id: MTTest.java,v 1.1 2002/07/11 20:58:40 dustin Exp $

package net.spy.test;

import java.util.Vector;
import java.util.Enumeration;

import junit.framework.TestCase;

import net.spy.util.ThreadPool;

/**
 * Test case that runs stuff in parallel.
 */
public class MTTest extends TestCase {

	/**
	 * Get an instance of MTTest.
	 */
	public MTTest(String name) {
		super(name);
	}

	/**
	 * Run the given task
	 *
	 * @param fac MTTaskFactory that will be providing instances of MTTask
	 * @param nRuns The number of MTTasks to instantiate and run
	 * @param nThreads The number of threads to use (how parallel to run
	 * 	the test)
	 */
	protected void runParallel(MTTaskFactory fac, int nRuns, int nThreads) {
		Vector v=new Vector();

		for(int i=0; i<nRuns; i++) {
			try {
				v.addElement(fac.newInstance());
			} catch(Throwable t) {
				t.printStackTrace();
				fail("Could not create MTTask instance:  " + t);
			}
		}

		runParallel(v.elements(), nThreads);
	}

	/**
	 * Run the given set of tasks over the given number of threads.
	 *
	 * @param tasks enumeration of MTTask objects
	 * @param nThreads number of tasks to perform simultaneously
	 */
	protected void runParallel(Enumeration tasksE, int nThreads) {
		boolean shutdown=false;
		Vector tasks=new Vector();

		// Check the arguments and copy them over to a Vector.  We do this
		// here so that we don't have to start the thread pool if the
		// arguments are bad.
		while(tasksE.hasMoreElements()) {
			MTTask t=(MTTask)tasksE.nextElement();
			tasks.addElement(t);
		}

		// Get the thread pool to run the tests
		ThreadPool tp=new ThreadPool(getName() + " test pool", nThreads);

		try {
			boolean finished=false;

			// Get all the tasks
			for(Enumeration e=tasks.elements(); e.hasMoreElements();) {
				MTTask t=(MTTask)e.nextElement();
				tp.addTask(t);
			}

			while(!finished) {
				// If all of the tasks have been accepted, tell the pool we
				// won't be sending in anymore
				if(tp.getTaskCount()==0 && !shutdown) {
					tp.shutdown();
					shutdown=true;
				}

				// If all of the threads are gone, we're finished.
				if(tp.getActiveThreadCount() == 0) {
					finished=true;
				}

				// Check for errors
				int errors=0;
				Throwable lastError=null;
				for(Enumeration e=tasks.elements(); e.hasMoreElements();) {
					MTTask mt=(MTTask)e.nextElement();
					errors+=mt.getFailureCount();
					lastError=mt.getLastFailure();
				}

				// If there were any errors, abort
				if(errors>0) {
					// First, shut down all the currently running tasks
					for(Enumeration e=tasks.elements(); e.hasMoreElements();) {
						MTTask mt=(MTTask)e.nextElement();
						mt.shutDown();
					}

					// Now, pass the error back up.
					String msg=null;
					if(lastError!=null) {
						msg=lastError.getMessage();
					}

					if(msg==null) {
						msg="Encountered " + errors
							+ " unknown error(s) while processing";
					}

					fail(msg);
				}
			}

		} finally {
			if(!shutdown) {
				tp.shutdown();
			}
		} // Make sure the pool gets shut down
	} // runParallel

}
