//
// $Id: GarbageCollector.java,v 1.2 2002/07/10 20:11:13 dustin Exp $

package net.spy.pool;

/**
 * Perform garbage collection with rate control.
 */
public class GarbageCollector extends Object {

	private static GarbageCollector gcInstance=null;

	private long lastRun=0;
	// Minimum time between calls.
	private static final int MIN_SLEEP=5000;

	private boolean inProgress=false;

	private GarbageCollector() {
		super();
	}

	/**
	 * Get the collector.
	 */
	public static synchronized GarbageCollector getGarbageCollector() {
		if(gcInstance==null) {
			gcInstance=new GarbageCollector();
		}
		return(gcInstance);
	}

	/**
	 * Run the garbage collection and perform finalization.
	 */
	public synchronized void collect() {
		long now=System.currentTimeMillis();

		if( (!inProgress) && (now - lastRun) > MIN_SLEEP ) {
			inProgress=true;
			try {
				System.gc();
				System.runFinalization();
			} finally {
				// Make sure we mark us as not being in progress
				inProgress=false;
			}
			lastRun=now;
		} else {
			System.err.println("Too soon for a garbage collection!");
		}
	}
}
