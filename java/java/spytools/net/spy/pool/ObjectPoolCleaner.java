// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPoolCleaner.java,v 1.7 2000/07/26 08:49:58 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

/**
 * This is a utility class used by ObjectPool to do periodic pool
 * management type things.
 */
public class ObjectPoolCleaner extends Thread {

	ObjectPool op=null;

	public ObjectPoolCleaner(ObjectPool op) {
		super();
		this.op=op;
		setDaemon(true);
		setName("ObjectPoolCleaner");
		start();
	}

	protected void doPrune() throws Exception {
		op.prune();
	}

	public void run() {
		while(true) {
			try {
				// Prune every once in a while.
				sleep(10*60*1000);
				doPrune();
			} catch(Exception e) {
				System.err.println("Cleaner got an exception:  " + e);
			}
		}
	}
}
