/*
 * Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyLogQueue.java,v 1.2 2000/07/19 23:36:39 dustin Exp $
 */

package net.spy.log;

import java.util.Vector;

/**
 * This class performs the actual queue management for the SpyLog system.
 * It should probably not be referenced directly.
 */

public class SpyLogQueue extends Object {
	protected static Vector log_buffer=null;
	protected final static String log_mutex="Log Mutex";

	public SpyLogQueue() {
		super();

		init();
	}

	protected synchronized void init() {
		synchronized(log_mutex) {
			if(log_buffer==null) {
				log_buffer=new Vector();
			}
		}
	}

	/**
	 * Add a new item to the queue.
	 *
	 * @SpyLogEntry item to be added.
	 */
	public void addToQueue(SpyLogEntry e) {
		synchronized(log_mutex) {
			log_buffer.addElement(e);
			log_mutex.notify();
		}
	}

	/**
	 * Wait for notification of an addition in the queue.
	 *
	 * @param ms The maximum number of milliseconds to wait.
	 */
	public void waitForQueue(long ms) {
		int size=0;

		// Figure out the size first.
		synchronized(log_mutex) {
			size=log_buffer.size();
		}

		if(size<=0) {
			// Only do this if we don't already think we have data
			try {
				synchronized(log_mutex) {
					log_mutex.wait(ms);
				}
			} catch(InterruptedException e) {
				// If we are going to return too early, pause just a sec
				System.err.println("SpyLogQueue.waitForQueue got exception:  "
					+ e);
				try {
					Thread.sleep(1000);
				} catch(InterruptedException e2) {
					System.err.println("Wow, this sucks, got another one: "
						+ e2);
				}
			}
		} // if we have no data
	}

	/**
	 * Flush the current log entries -- DO NOT CALL THIS.  This is for
	 * SpyLogFlushers only.
	 */
	public Vector flush() {
		Vector ret=null;
		synchronized(log_mutex) {
			ret=log_buffer;          // Copy the old vector's reference.
			log_buffer=new Vector(); // Create a new one.
		}
		return(ret);
	}
}
