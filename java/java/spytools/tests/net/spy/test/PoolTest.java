// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PoolTest.java,v 1.6 2002/07/11 23:22:13 dustin Exp $

package net.spy.test;

import java.util.*;

import junit.framework.*;

import net.spy.SpyConfig;
import net.spy.pool.*;
import net.spy.test.*;

/**
 * Test suite for pooling stuff.
 */
public class PoolTest extends MTTest {

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
			op.createPool("test", new TestPoolFiller("test", conf));
		} catch(PoolException pe) {
			pe.printStackTrace();
			fail("Couldn't create the pool.");
		}

		// poolPrinter=new PoolPrinter(op);
	}

	protected void tearDown() {
		if(poolPrinter!=null) {
			System.err.println("Shutting down the pool printer");
			poolPrinter.shutDown();
		}
		try {
			op.destroyPool("test");
		} catch(PoolException pe) {
			fail("Couldn't destroy pool.");
		}
	}

	public void testFetch250WithoutClosing() {
		mtTest("testFetch250WithoutClosing", false, 10, 25, 10);
	}

	public void testFetch1000WithClosing() {
		mtTest("testFetch1000WithClosing", true, 10, 100, 10);
	}

	public void testFetch2500BigWithoutClosing() {
		mtTest("testFetch2500BigWithoutClosing", false, 250, 250, 10);
	}

	public void testFetch10000BigWithClosing() {
		mtTest("testFetch10000BigWithClosing", true, 250, 1000, 10);
	}

	private void mtTest(String name, boolean close,
		int threads, int tasks, int each) {

		// We can't use the factory because the test are inner classes.
		// MTTaskFactory fac=new MTTaskFactory(touse, each, true);

        Vector v=new Vector();
        for(int i=0; i<tasks; i++) {
            MTTask t=null;
            if(close) {
                t=new PoolTestMTTask(each, true);
            } else {
                t=new PoolTestMTTaskNoReturn(each, true);
            }
            v.addElement(t);
        }

		// Run the parallel test
		runParallel(v.elements(), threads);

        // Count the successes, just to be sure
        int successes=0;
        for(Enumeration e=v.elements(); e.hasMoreElements();) {
            PoolTestMTTask t=(PoolTestMTTask)e.nextElement();
            successes+=t.getSuccesses();
        }

        int expected=(tasks*each);
        assertTrue("Expected " + expected + " successes, got " + successes,
            (successes == expected));
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

	public class PoolTestMTTask extends MTTask {

		private int successes=0;
		protected Random r=null;

		public PoolTestMTTask(int iterations, boolean stopOnFailure) {
			super(iterations, stopOnFailure);
			r=new Random();
		}

		// Return the number of times the test was successful.
		public int getSuccesses() {
			return(successes);
		}

		/**
		 * Perform the operation on the object pool.
		 */
		protected void poolOp() throws InterruptedException, PoolException {

			ObjectPool op=new ObjectPool(new SpyConfig());
			PooledObject po=op.getObject("test");
			Thread.sleep(r.nextInt(100));
			po.checkIn();
		}

		public void performTest() {
			try {
				poolOp();
				successes++;
			} catch(InterruptedException ie) {
				addFailure(ie);
			} catch(PoolException pe) {
				addFailure(pe);
			}
		}
	}

	// Same as above, but don't check the object back in
	public class PoolTestMTTaskNoReturn extends PoolTestMTTask {
		public PoolTestMTTaskNoReturn(int iterations, boolean stopOnFailure) {
			super(iterations, stopOnFailure);
		}

		protected void poolOp() throws InterruptedException, PoolException {
			ObjectPool op=new ObjectPool(new SpyConfig());
			PooledObject po=op.getObject("test");
			Thread.sleep(r.nextInt(100));
		}

	}

}
