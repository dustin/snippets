/*
 * Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyLogQueue.java,v 1.6 2000/11/02 22:19:42 dustin Exp $
 */

package net.spy.log;

import java.util.Vector;
import java.util.Hashtable;

/**
 * This class performs the actual queue management for the SpyLog system.
 * It should probably not be referenced directly.
 */

public class SpyLogQueue extends Object {
	protected static Hashtable queues=null;
	protected static String queue_mutex="Log Mutex";
	protected String queue_name=null;

	/**
	 * @deprecated Please specify a queue name.
	 */
	public SpyLogQueue() {
		this("GenericSpyLogQueue");
		Exception e=new Exception("Using deprecated API!");
		System.err.println("Warning!  " + e);
		e.printStackTrace();
	}

	public SpyLogQueue(String name) {
		super();
		this.queue_name=name;
		init();
	}

	protected void init() {
		synchronized(queue_mutex) {
			if(queues==null) {
				queues=new Hashtable();
			}

			Vector v=(Vector)queues.get(queue_name);
			if(v==null) {
				v=new Vector();
				queues.put(queue_name, v);
			}
		}
	}

	protected Vector getQueue() {
		Vector v=null;
		synchronized(queue_mutex) {
			v=(Vector)queues.get(queue_name);
		}
		return(v);
	}

	/**
	 * Add a new item to a queue.
	 *
	 * @param e item to be added
	 */
	public void addToQueue(SpyLogEntry e) {
		Vector v=getQueue();
		synchronized(v) {
			v.addElement(e);
		}
		synchronized(queue_mutex) {
			queue_mutex.notify();
		}
	}

	/**
	 * Wait for notification of an addition in the queue.
	 *
	 * @param ms The maximum number of milliseconds to wait.
	 */
	public void waitForQueue(long ms) {
		int size=0;
		Vector log_buffer=getQueue();

		// Figure out the size first.
		synchronized(queue_mutex) {
			size=log_buffer.size();
		}

		if(size<=0) {
			// Only do this if we don't already think we have data
			try {
				synchronized(queue_mutex) {
					queue_mutex.wait(ms);
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
		Vector log_buffer=getQueue();
		synchronized(queue_mutex) {
			size=log_buffer.size();
		}
		return(size);
	}

	/**
	 * Flush the current log entries -- DO NOT CALL THIS.  This is for
	 * SpyLogFlushers only.
	 */
	public Vector flush() {
		Vector ret=null;
		Vector log_buffer=getQueue();
		synchronized(queue_mutex) {
			ret=log_buffer;          // Copy the old vector's reference.
			log_buffer=new Vector(); // Create a new one.
			queues.put(queue_name, log_buffer);
		}
		return(ret);
	}
}
