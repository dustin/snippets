// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPoolCleaner.java,v 1.8 2000/10/13 06:50:21 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

/**
 * This is a utility class used by ObjectPool to do periodic pool
 * management type things.
 */
public class ObjectPoolCleaner extends Thread {

	// The object pool reference we'll be cleaning.
	protected ObjectPool op=null;

	// How many times we've cleaned so far.
	protected int numCleans=0;

	/**
	 * Create (and start) the ObjectPoolCleaner.
	 */
	public ObjectPoolCleaner(ObjectPool op) {
		super();
		this.op=op;
		setDaemon(true);
		setName("ObjectPoolCleaner");
		start();
	}

	/**
	 * Look like a normal thread, but report number of times the thing's
	 * cleaned.
	 */
	public String toString() {
		return(super.toString() + " - " + numCleans + " served.");
	}

	protected void doPrune() throws Exception {
		op.prune();
		numCleans++;
	}

	public void run() {
		// Only do six cleans (sleeping ten minutes, that's an hour!)
		while(numCleans<6) {
			try {
				// Prune every once in a while.
				sleep(10*60*1000);
				doPrune();
			} catch(Exception e) {
				System.err.println("***\nCleaner got an exception:  "
					+ e + "\n***");
			}
		}
	}
}
