// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PoolTest.java,v 1.4 2002/07/10 20:45:27 dustin Exp $

package net.spy.test;

import java.util.*;

import junit.framework.*;

import net.spy.SpyConfig;
import net.spy.util.*;
import net.spy.pool.*;

/**
 * Test suite for pooling stuff.
 */
public class PoolTest extends TestCase {

	private ObjectPool op=null;
	private PoolPrinter poolPrinter=null;

	public PoolTest(String name) {
		super(name);
	}

	public static Test suite() {
		return new TestSuite(PoolTest.class);
	}

	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	// Set up the test.
	protected void setUp() {
		SpyConfig conf=new SpyConfig();
		conf.put("test.min", "0");
		conf.put("test.start", "100");
		conf.put("test.max", "100");
		conf.put("test.yellow", "75");
		conf.put("test.max_age", "300000");
		op=new ObjectPool(conf);
		try {
			synchronized(op.getClass()) {
				if(!op.hasPool("test")) {
					op.createPool("test", new TestPoolFiller("test", conf));
				}
			}
		} catch(PoolException pe) {
			pe.printStackTrace();
			fail("Couldn't create the pool.");
		}

		poolPrinter=new PoolPrinter(op);
	}

	protected void tearDown() {
		System.err.println("Shutting down the pool printer");
		poolPrinter.shutDown();
	}

	public void testFetch250WithoutClosing() {
		mtTest("testFetch1000WithClosing", false, 10, 25, 10);
	}

	public void testFetch1000WithClosing() {
		mtTest("testFetch1000WithClosing", true, 10, 100, 10);
	}

	public void testFetch2500BigWithoutClosing() {
		mtTest("testFetch1000WithClosing", false, 250, 250, 10);
	}

	public void testFetch10000BigWithClosing() {
		mtTest("testFetch1000WithClosing", true, 250, 1000, 10);
	}

	private void mtTest(String name, boolean close,
		int threads, int tasks, int each) {
		ThreadPool tp=new ThreadPool(name, threads);
		Vector v=new Vector();
		for(int i=0; i<tasks; i++) {
			v.addElement(new TestTask(each, close));
		}
		for(Enumeration e=v.elements(); e.hasMoreElements();) {
			tp.addTask((Runnable)e.nextElement());
		}
		try {
			// Wait for all jobs to finish, *or* for an error to occur.
			boolean finished=false;
			boolean shutdown=false;
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
				for(Enumeration e=v.elements(); e.hasMoreElements();) {
					TestTask tt=(TestTask)e.nextElement();
					errors+=tt.getFailures();
				}
				assertTrue(errors == 0);

				// Wait a second before looping
				Thread.sleep(1000);
			}
		} catch(InterruptedException ie) {
			ie.printStackTrace();
			fail("Interrupted");
		}

		// Count the successes
		int successes=0;
		for(Enumeration e=v.elements(); e.hasMoreElements();) {
			TestTask tt=(TestTask)e.nextElement();
			// Make sure the tests stop running
			tt.shutDown();
			successes+=tt.getSuccesses();
		}

		// Make sure there were enough successes
		assertTrue(successes == (each * tasks));
	}

	// Private support classes

	// Print the object pool every five seconds.
	private class PoolPrinter extends Thread {

		private ObjectPool op=null;
		private boolean keepGoing=true;

		public PoolPrinter(ObjectPool op) {
			super();
			this.op=op;
			this.setDaemon(true);
			this.setName("PoolPrinter");
			this.start();
		}

		public void shutDown() {
			keepGoing=false;
		}

		public void run() {
			while(keepGoing) {
				try {
					sleep(5000);

					System.out.println(op);
				} catch(InterruptedException ie) {
					ie.printStackTrace();
				}
			}
		}
	}

	// Actual poolable
	private class TestPoolable extends PoolAble {
		// get the poolable
		public TestPoolable(Object o, int hash) {
			super(o, hash);
		}
	}

	// Actual pool filler
	private class TestPoolFiller extends PoolFiller {
		public TestPoolFiller(String name, SpyConfig conf) {
			super(name, conf);
		}

		public PoolAble getObject() throws PoolException {
			return(new TestPoolable(new Object(), getClass().hashCode()));
		}
	}

	// Thread pool task for MT testing
	private class TestTask extends Object implements Runnable {
		private int failures=0;
		private int success=0;
		private int runs=0;
		private boolean checkBackIn=false;
		private boolean keepGoing=true;

		public TestTask(int runs, boolean checkBackIn) {
			super();

			this.runs=runs;
			this.checkBackIn=checkBackIn;
		}

		public int getFailures() {
			return(failures);
		}

		public int getSuccesses() {
			return(success);
		}

		public void shutDown() {
			keepGoing=false;
		}

		public void run() {
			Random r=new Random();
			ObjectPool op=new ObjectPool(new SpyConfig());
			for(int i=0; keepGoing && i<runs; i++) {
				try {
					PooledObject po=op.getObject("test");

					Thread.sleep(r.nextInt(100));

					if(checkBackIn) {
						po.checkIn();
					}

					success++;
				} catch(InterruptedException ie) {
					ie.printStackTrace();
					failures++;
				} catch(PoolException pe) {
					pe.printStackTrace();
					failures++;
				}
			}
		}
	} // testTask

}
