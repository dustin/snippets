//
// $Id: GarbageCollector.java,v 1.3 2002/07/11 07:56:06 dustin Exp $

package net.spy.pool;

/**
 * Perform garbage collection with rate control.
 *
 * Debug messages may be printed if the following system property is set:<br/>
 * <code>net.spy.pool.GarbageCollector.debug</code><br/>
 * The value of the property must be either <code>message</code> for simple
 * messages or <code>stack</code> for stack traces whenever two things try
 * to request a stack trace with too little time between them.
 */
public class GarbageCollector extends Object {

	private static GarbageCollector gcInstance=null;

	private long lastRun=0;
	// Minimum time between calls.
	private static final int MIN_SLEEP=5000;

	private boolean inProgress=false;

	private boolean debugStack=false;
	private boolean debugMessage=false;

	private GarbageCollector() {
		super();

		String ds=System.getProperty("net.spy.pool.GarbageCollector.debug");
		if(ds!=null) {
			if(ds.equals("stack")) {
				debugStack=true;
			}
			if(ds.equals("message")) {
				debugMessage=true;
			}
		}
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
			if(debugMessage) {
				System.err.println("Too soon for a garbage collection!");
			}
			if(debugStack) {
				new GCWarning(
					"Too soon for a garbage collection!").printStackTrace();
			}
		}
	}

	private class GCWarning extends Exception {
		public GCWarning(String msg) {
			super(msg);
		}
	}
}
