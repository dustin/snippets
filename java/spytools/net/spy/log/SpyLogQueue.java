/*
 * Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyLogQueue.java,v 1.10 2002/07/10 05:41:44 dustin Exp $
 */

package net.spy.log;

import java.util.Hashtable;
import java.util.Vector;

/**
 * This class performs the actual queue management for the SpyLog system.
 * It should probably not be referenced directly.
 */
public class SpyLogQueue extends Object {
	private static Hashtable queues=null;
	private static String queueMutex="Log Mutex";
	private String queueName=null;

	/**
	 * Get a new SpyLogQueue with the given name.
	 */
	public SpyLogQueue(String name) {
		super();
		this.queueName=name;
		init();
	}

	private void init() {
		synchronized(queueMutex) {
			if(queues==null) {
				queues=new Hashtable();
			}

			Vector v=(Vector)queues.get(queueName);
			if(v==null) {
				v=new Vector();
				queues.put(queueName, v);
			}
		}
	}

	private Vector getQueue() {
		Vector v=null;
		synchronized(queueMutex) {
			v=(Vector)queues.get(queueName);
		}
		return(v);
	}

	/**
	 * Add a new item to a queue.
	 *
	 * @param e item to be added
	 */
	public void addToQueue(SpyLogEntry e) {
		synchronized(queueMutex) {
			Vector v=getQueue();
			synchronized(v) {
				v.addElement(e);
				v.notify();
			}
			queueMutex.notify();
		}
	}

	/**
	 * Wait for notification of an addition in the queue.
	 *
	 * @param ms The maximum number of milliseconds to wait.
	 */
	public void waitForQueue(long ms) {
		if(size()<=0) {
			// Only do this if we don't already think we have data
			try {
				synchronized(queueMutex) {
					queueMutex.wait(ms);
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
	 * Return the current size of the queue.  This may change before you
	 * can do anything about it, so use it wisely.
	 */
	public int size() {
		int size=-1;
		synchronized(queueMutex) {
			Vector logBuffer=getQueue();
			size=logBuffer.size();
		}
		return(size);
	}

	/**
	 * Flush the current log entries -- DO NOT CALL THIS.  This is for
	 * SpyLogFlushers only.
	 */
	public Vector flush() {
		Vector ret=null;
		synchronized(queueMutex) {
			Vector logBuffer=getQueue();
			ret=logBuffer;          // Copy the old vector's reference.
			logBuffer=new Vector(); // Create a new one.
			queues.put(queueName, logBuffer);
		}
		return(ret);
	}
}
