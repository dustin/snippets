/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLog.java,v 1.3 2000/07/27 19:21:46 dustin Exp $
 */

package net.spy.log;

import net.spy.*;

import java.lang.*;
import java.util.*;

/**
 * The Spy Asyncrhonous logger.
 * <p>
 * SpyLog is an implementation of an asynchronous logger that allows one to
 * have multiple points of input into a single queueing system that runs in
 * its own thread recording the logs.
 * <p>
 * If this doesn't sound immediately obvious to you, imagine having a
 * transactional system that logs into a database without slowing down the
 * transactions.  SpyLog gives you the ability to get a log entry out of your
 * way quickly, to be permanently recorded later.
 */

public class SpyLog extends Object {
	protected SpyLogQueue queue=null;
	protected static boolean initialized = false;
	protected static Vector flushers=null;
	protected String queue_name="GenericSpyLogQueue";

	/**
	 * Instantiate a SpyLog entry with the default flusher.
	 * @deprecated Please specify a queue name
	 */
	public SpyLog() {
		this("GenericSpyLogQueue");
	}


	/**
	 * Instantiate a SpyLog interface for the given queue name with the
	 * default flusher.
	 */
	public SpyLog(String queue_name) {
		super();

		this.queue_name=queue_name;

		// Important to initialize only once, this sets up all the static
		// variables including the cleanup thread.
		if(initialized == false) {
			initialize();

			// If this is initialization, and we don't have a flusher, make
			// one.
			synchronized(flushers) {
				// Default flusher.
				if(flushers.size()==0) {
					SpyLogFlusher flusher = new SpyLogFlusher();
					addFlusher(flusher);
				}
			}
		}

		// Grab a queue object
		queue=new SpyLogQueue(queue_name);
	}
	/**
	 * Instantiate a SpyLog entry with an alternative log flusher.  An
	 * alternative log flusher may log into a SQL database, or to a pager,
	 * or email, etc...
	 *
	 * @deprecated Please specify a queue name.
	 */
	public SpyLog(SpyLogFlusher f) {
		this("GenericSpyLogQueue", f);
	}

	/**
	 * Instantiate a SpyLog entry with an alternative log flusher.  An
	 * alternative log flusher may log into a SQL database, or to a pager,
	 * or email, etc...
	 */
	public SpyLog(String queue_name, SpyLogFlusher f) {
		super();

		this.queue_name=queue_name;

		// Important to initialize only once, this sets up all the static
		// variables including the cleanup thread.
		if(initialized == false) {
			initialize();
		}

		// The log flusher object.
		addFlusher(f);

		// Grab a queue object
		queue=new SpyLogQueue(queue_name);
	}

	/**
	 * Add another log flusher to the pool.
	 */
	public void addFlusher(SpyLogFlusher f) {
		synchronized(flushers) {
			try {
				f.start();
			} catch(IllegalThreadStateException e) {
				// We don't care if it's already started.
			}
			flushers.addElement(f);
		}
	}

	/**
	 * Log an entry.
	 *
	 * @param msg SpyLogEntry object to be logged.
	 */
	public void log(SpyLogEntry msg) {
		queue.addToQueue(msg);
	}

	protected synchronized void initialize() {
		// Do this soon, we don't want anything else causing this to happen.
		initialized = true;

		if(flushers==null) {
			flushers=new Vector();
		}

		// Really need to make sure all finalization occurs.
		System.runFinalizersOnExit(true);
	}

	protected void finalize() throws Throwable {
		super.finalize();
	}
}
