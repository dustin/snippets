// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ThreadPool.java,v 1.1 2001/07/28 03:15:28 dustin Exp $

package net.spy.util;

import java.util.*;

/**
 * A thread pool for easy parallelism.
 */
public class ThreadPool extends Object {

	// The threads we're managing.
	private Vector threads=null;
	// The tasks for the threads to do.
	private Stack tasks=null;

	// Private thread ID allocator for the inner class.
	private static int thread_ids=0;

	/**
	 * Get an instance of ThreadPool.
	 *
	 * @param n the number of threads to pool
	 */
	public ThreadPool(int n) {
		super();
		tasks=new Stack();
		threads=new Vector();

		// Initialize all of the threads.
		for(int i=0; i<n; i++) {
			RunThread rt=new RunThread(tasks, this);
			threads.addElement(rt);
		}
	}

	/**
	 * Add a task for one of the threads to execute.
	 */
	public void addTask(Runnable r) {
		synchronized(tasks) {
			tasks.addElement(r);
			tasks.notify();
		}
	}

	/**
	 * Find out how many tasks are in the queue.
	 */
	public int getTaskCount() {
		int rv=0;
		synchronized(tasks) {
			rv=tasks.size();
		}
		return(rv);
	}

	/**
	 * Find out how many threads are in the pool.
	 */
	public int getThreadCount() {
		return(threads.size());
	}

	/**
	 * Tell all the threads to shut down after they finish their current
	 * tasks.
	 */
	public void shutdown() {
		for(Enumeration e=threads.elements(); e.hasMoreElements(); ) {
			RunThread t=(RunThread)e.nextElement();
			t.shutdown();
		}
	}

	/**
	 * Wait until there are no more than <i>num</i> tasks.
	 */
	public void waitForTaskCount(int num) throws InterruptedException {
		while(getTaskCount() > num) {
			synchronized(this) {
				wait(5000);
			}
		}
	}

	// A test task that takes a random amount of time.
	private static Runnable getTestRunnable() {
		return(new Runnable() {
			public void run() {
				try {
					Random rand=new Random();
					long l=Math.abs(rand.nextLong()%15000);
					System.err.println("Sleeping for " + l);
					Thread.sleep(l);
					System.err.println("Done!");
				} catch(Exception e) {
					e.printStackTrace();
				}
			}
			});
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		ThreadPool tp=new ThreadPool(15);

		// Toss a hunded tasks into the thing.
		for(int i=0; i<100; i++) {
			tp.addTask(getTestRunnable());
		}

		// Add another 100 tasks, but only if the task count is below 50
		for(int i=0; i<100; i++) {
			tp.waitForTaskCount(50);
			System.err.println("Adding new task.");
			tp.addTask(getTestRunnable());
		}

		// Wait for all of the tasks to finish
		tp.waitForTaskCount(0);
		// I shut 'em down!
		tp.shutdown();

	}

	// //////////////////////////////////////////////////////////////////////
	// The threads that make up the pool.
	// //////////////////////////////////////////////////////////////////////

	private class RunThread extends Thread {
		private Object monitor=null;
		private Stack tasks=null;
		private boolean going=true;
		private int thread_id=0;

		public RunThread(Stack tasks, Object monitor) {
			super();

			this.tasks=tasks;
			this.monitor=monitor;

			thread_id=thread_ids++;

			System.err.println("RunThread " + thread_id + " going online.");

			setName("RunThread#" + thread_id);
			// This should not be a daemon thread.
			start();
		}

		// I shut 'em down!
		public void shutdown() {
			going=false;
		}

		private void run(Runnable r) {
			try {
				// Run the runnable.
				r.run();
			} catch(Throwable t) {
				t.printStackTrace();
			}
		}

		public void run() {
			while(going) {
				try {
					Runnable r=(Runnable)tasks.pop();
					// Let the monitor know we got one.
					synchronized(monitor) {
						monitor.notify();
					}
					run(r);
				} catch(EmptyStackException e) {
					// If the stack is empty, wait for something to get added.
					synchronized(tasks) {
						try {
							// Wait up to ten seconds
							tasks.wait(10000);
						} catch(InterruptedException ie) {
							// That's OK, we'll try again.
						}
					}
				} // empty stack
			} // while
		} // ThreadPool$RunThread.run()
	} // ThreadPool$RunThread
} // ThreadPool
