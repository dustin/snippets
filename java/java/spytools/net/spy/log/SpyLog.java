/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLog.java,v 1.1 2000/07/19 03:22:33 dustin Exp $
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
	protected static SpyLogFlusher flusher=null;
	protected static int refcount=0;

	/**
	 * Instantiate a SpyLog entry.
	 */
	public SpyLog() {
		super();
		// Important to initialize only once, this sets up all the static
		// variables including the cleanup thread.
		if(initialized == false) {
			initialize();
		}

		// Grab a queue object
		queue=new SpyLogQueue();

		// Count of the number of objects out there, when there aren't any
		// more, we stop the cleaner thread and force a shutdown.
		refcount++;
	}

	/**
	 * Instantiate a SpyLog entry with an alternative log flusher.  An
	 * alternative log flusher may log into a SQL database, or to a pager,
	 * or email, etc...
	 */
	public SpyLog(SpyLogFlusher f) {
		super();

		// The log flusher object.
		flusher=f;

		// Important to initialize only once, this sets up all the static
		// variables including the cleanup thread.
		if(initialized == false) {
			initialize();
		}

		// Grab a queue object
		queue=new SpyLogQueue();

		// Count of the number of objects out there, when there aren't any
		// more, we stop the cleaner thread and force a shutdown.
		refcount++;
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
		refcount = 0;

		if(flusher==null) {
			flusher = new SpyLogFlusher();
		}
		flusher.start();

		// Really need to make sure all finalization occurs.
		System.runFinalizersOnExit(true);
	}

	protected void finalize() throws Throwable {
		// One fewer reference.
		refcount--;
		super.finalize();
	}
}
