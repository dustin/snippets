//
// $Id: GarbageCollector.java,v 1.1 2002/07/10 20:00:28 dustin Exp $

package net.spy.pool;

/**
 * Perform garbage collection with rate control.
 */
public class GarbageCollector extends Object {

	private static GarbageCollector gcInstance=null;

	private long lastRun=0;
	// Minimum time between calls.
	private static final int MIN_SLEEP=5000;

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
	public void collect() {
		long now=System.currentTimeMillis();

		if( (now - lastRun) > MIN_SLEEP ) {
			System.gc();
			System.runFinalization();
			lastRun=now;
		} else {
			System.err.println("Too soon for a garbage collection!");
		}
	}
}
