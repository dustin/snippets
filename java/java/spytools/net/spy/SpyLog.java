/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLog.java,v 1.2 2000/01/24 06:40:32 dustin Exp $
 */

package net.spy;

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
	protected static Vector log_buffer[];
	protected static int current_buffer;
	protected static boolean initialized = false;
	protected static SpyLogFlusher flusher;
	protected static int refcount;
	protected static ThreadGroup mythreadgroup;

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
		log_buffer[current_buffer].addElement(msg);
	}

	/**
	 * Flush the current log entries -- DO NOT CALL THIS.  This is for
	 * internal use only, and should only be called by a SpyLogFlusher.
	 */
	public synchronized Vector flush() {
		int last_buffer = current_buffer;
		if(current_buffer == 0) {
			current_buffer=1;
		} else {
			current_buffer=0;
		}
		log_buffer[current_buffer] = new Vector();
		return(log_buffer[last_buffer]);
	}

	protected synchronized void initialize() {
		ThreadGroup system = getSystemGroup();
		// Do this soon, we don't want anything else causing this to happen.
		initialized = true;
		refcount = 0;

		mythreadgroup = new ThreadGroup(system, "logging");
		if(flusher==null) {
			flusher = new SpyLogFlusher(mythreadgroup);
		}
		flusher.start();
		// Sleep a tiny bit so the thread can get going, otherwise,
		// finalization will cause all buffered logs to be lost if it's
		// called before the thread officially starts.  This should be
		// pretty damned rare (I forced it to happen in testing), but it is
		// a possibility.
		try {
			Thread.sleep(10);
		} catch(InterruptedException e) {
		}

		log_buffer=new Vector[2];
		log_buffer[0]=new Vector();
		log_buffer[1]=new Vector();

		current_buffer = 0;

		// Really need to make sure all finalization occurs.
		System.runFinalizersOnExit(true);
	}

	protected void finalize() throws Throwable {
		// One fewer reference.
		refcount--;
		super.finalize();
	}

	// Get the system threadgroup for initialize
	protected static ThreadGroup getSystemGroup() {
		ThreadGroup start=null, last=null;

		for(start=Thread.currentThread().getThreadGroup(); start!=null;) {
			last=start;
			start=start.getParent();
		}
		return(last);
	}
}
