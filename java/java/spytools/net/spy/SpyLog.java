/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLog.java,v 1.1 1999/10/20 02:25:36 dustin Exp $
 */

package net.spy;

import java.lang.*;
import java.util.*;

public class SpyLog extends Object {
	protected static Vector log_buffer[];
	protected static int current_buffer;
	protected static boolean initialized = false;
	protected static SpyLogFlusher flusher;
	protected static int refcount;
	protected static ThreadGroup mythreadgroup;

	// Actual log implementation.  Fairly simple, just add it to the proper
	// vector.
	public void log(SpyLogEntry msg) {
		log_buffer[current_buffer].addElement(msg);
	}

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

	// This should only be used by the cleanup thread.
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

	public void finalize() throws Throwable {
		// One fewer reference.
		refcount--;
		super.finalize();
	}

	// Get the system threadgroup for initialize
	public static ThreadGroup getSystemGroup() {
		ThreadGroup start=null, last=null;

		for(start=Thread.currentThread().getThreadGroup(); start!=null;) {
			last=start;
			start=start.getParent();
		}
		return(last);
	}
}
