// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PoolTest.java,v 1.1 2002/07/10 07:36:09 dustin Exp $

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
	}

	public void testFetch250WithoutClosing() {
		mtTest("testFetch1000WithClosing", false, 10, 25, 10);
	}

	public void testFetch1000WithClosing() {
		mtTest("testFetch1000WithClosing", true, 10, 100, 10);
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
			tp.waitForTaskCount(0);
			tp.shutdown();
			tp.waitForThreads();
		} catch(InterruptedException ie) {
			ie.printStackTrace();
			fail("Interrupted");
		}

		int errors=0;
		int successes=0;
		for(Enumeration e=v.elements(); e.hasMoreElements();) {
			TestTask tt=(TestTask)e.nextElement();
			errors+=tt.getFailures();
			successes+=tt.getSuccesses();
		}

		if(errors>0) {
			fail("There were " + errors + " failures");
		}

		assertTrue(successes == (each * tasks));
	}

	// Private support classes

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

		public void run() {
			Random r=new Random();
			ObjectPool op=new ObjectPool(new SpyConfig());
			for(int i=0; i<runs; i++) {
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
