// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ThreadPool.java,v 1.4 2002/02/24 07:25:57 dustin Exp $

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

	// This is what we monitor for things being checked out (otherwise we
	// can't tell the difference between adds and check outs).
	private Object monitor=null;

	// Private thread ID allocator for the inner class.
	private static int thread_ids=0;

	/**
	 * Get an instance of ThreadPool.
	 *
	 * @param name Name of the pool.
	 * @param n Number of threads.
	 * @param prio Priority of the child threads.
	 */
	public ThreadPool(String name, int n, int priority) {
		super();

		if(priority<Thread.MIN_PRIORITY || priority>Thread.MAX_PRIORITY) {
			throw new IllegalArgumentException(priority
				+ " is an invalid priority.");
		}

		tasks=new Stack();
		threads=new Vector();
		monitor=new Object();

		// Get the group for this threadpool.
		ThreadGroup tg=new ThreadGroup(name);
		tg.setDaemon(true);

		// Initialize all of the threads.
		for(int i=0; i<n; i++) {
			RunThread rt=new RunThread(tg, tasks, monitor);
			rt.setPriority(priority);
			threads.addElement(rt);
		}
	}

	/**
	 * Get an instance of ThreadPool with a normal priority.
	 *
	 * @param name Name of the pool.
	 * @param n Number of threads.
	 */
	public ThreadPool(String name, int n) {
		this(name, n, Thread.NORM_PRIORITY);
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
			synchronized(monitor) {
				monitor.wait(5000);
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
	 * Shuts down in case you didn't.
	 */
	protected void finalize() throws Throwable {
		System.err.println(
			"********** Shutting down abandoned thread pool **********");
		shutdown();
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		ThreadPool tp=new ThreadPool("TestThreadPool", 15);

		// Toss a hunded tasks into the thing.
		for(int i=0; i<100; i++) {
			tp.addTask(getTestRunnable());
			Thread.currentThread().getThreadGroup().list();
		}

		// Add another 100 tasks, but only if the task count is below 50
		for(int i=0; i<100; i++) {
			tp.waitForTaskCount(50);
			System.out.println("Adding new task.");
			tp.addTask(getTestRunnable());
		}

		// Wait for all of the tasks to finish
		tp.waitForTaskCount(0);
		System.out.println("All tasks have been accepted, shutting down.");
		// I shut 'em down!
		tp.shutdown();

		System.out.println("Done.");
	}

	// //////////////////////////////////////////////////////////////////////
	// The threads that make up the pool.
	// //////////////////////////////////////////////////////////////////////

	private class RunThread extends Thread {
		private Object monitor=null;
		private Stack tasks=null;
		private boolean going=true;
		private int thread_id=0;

		private String runningMutex=null;
		private Object running=null;
		private long start=0;

		public RunThread(ThreadGroup tg, Stack tasks, Object monitor) {
			super(tg, "RunThread");

			runningMutex=new String("runningMutex");
			this.tasks=tasks;
			this.monitor=monitor;

			thread_id=thread_ids++;

			System.out.println("RunThread " + thread_id + " going online.");

			// Adjust the name to include the thread number
			setName("RunThread#" + thread_id);
			// Note:  This should not be a daemon thread.
			start();
		}

		public String toString() {
			StringBuffer sb=new StringBuffer();
			sb.append(super.toString());

			synchronized(runningMutex) {
				if(running==null) {
					sb.append(" - idle");
				} else {
					sb.append(" - running ");
					sb.append(running.getClass().getName());
					sb.append(" for ");
					sb.append(System.currentTimeMillis() - start);
					sb.append("ms");
				}
			}
			return(sb.toString());
		}

		// I shut 'em down!
		public void shutdown() {
			going=false;
		}

		private void run(Runnable r) {
			try {
				// Record the runnable
				running=r;
				start=System.currentTimeMillis();
				// Run the runnable.
				r.run();
			} catch(Throwable t) {
				t.printStackTrace();
			}
			synchronized(runningMutex) {
				running=null;
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
