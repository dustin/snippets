//
// $Id: PoolContainer.java,v 1.37 2002/08/16 07:27:08 dustin Exp $

package net.spy.pool;

import java.util.Iterator;
import java.util.ArrayList;

import net.spy.SpyConfig;

/**
 * PoolContainer is the storage for a given pool.
 */
public class PoolContainer extends Object {
	private ArrayList pool=null;
	private SpyConfig conf=null;
	private String name=null;
	private PoolFiller filler=null;
	private int minObjects=-1;
	private int initObjects=-1;
	private int maxObjects=-1;

	// The percentage at which we start making people wait before giving
	// them new connections.
	private int yellowLine=-1;

	private static int objectId=0;

	private PoolDebug pooldebug=null;

	/**
	 * Create a new PoolContainer for a pool with a given name, and filler.
	 *
	 * The following optional config parameters will be used:
	 * <ul>
	 *  <li>&lt;poolname&gt;.min - minimum number of items in the pool</li>
	 *  <li>&lt;poolname&gt;.start - initial number of objects in the pool</li>
	 *  <li>&lt;poolname&gt;.yellow - when the pool is this percent full,
	 *      we hesitate more before giving out connections.</li>
	 *  <li>&lt;poolname&gt;.max - maximum number of items in the pool</li>
	 * </li>
	 *
	 * @param name name of the pool
	 * @param pf the PoolFiller to use
	 * @param conf a SpyConfig object that should describe the pool
	 * parameters.
	 *
	 * @exception PoolException when something bad happens
	 */
	public PoolContainer(String name, PoolFiller pf, SpyConfig conf)
		throws PoolException {
		super();
		this.conf=conf;
		this.name=name;
		this.filler=pf;

		initialize();
	}

	/**
	 * Create a new PoolContainer for a pool with a given name, and filler.
	 *
	 * The following optional config parameters will be used:
	 * <ul>
	 *  <li>&lt;poolname&gt;.min - minimum number of items in the pool</li>
	 *  <li>&lt;poolname&gt;.start - initial number of objects in the pool</li>
	 *  <li>&lt;poolname&gt;.yellow - when the pool is this percent full,
	 *      we hesitate more before giving out connections.</li>
	 *  <li>&lt;poolname&gt;.max - maximum number of items in the pool</li>
	 * </li>
	 *
	 * @param name name of the pool
	 * @param pf the PoolFiller to use
	 *
	 * @exception PoolException when something bad happens
	 */
	public PoolContainer(String name, PoolFiller pf)
		throws PoolException {
		super();
		this.name=name;
		this.filler=pf;
		this.conf=pf.getConfig();

		initialize();
	}

	/**
	 * Get the name of the pool.
	 */
	public String getName() {
		return(name);
	}

	/**
	 * Get an object from the pool.  It could take up to about three
	 * seconds to get an object from the pool.
	 *
	 * @exception PoolException when something bad happens
	 */
	public PooledObject getObject() throws PoolException {
		PooledObject rv=null;
		PoolAble poolable=null;

		// How many times we're flipping through the object pool
		int retries=6;

		// Synchronize on the pool object.
		synchronized(pool) {
			// We'll try up to three seconds to get an object from the pool
			for(int retry=0; poolable==null && retry<retries; retry++) {

				// Find the next available object.
				for(Iterator e=pool.iterator();
					poolable==null && e.hasNext(); ) {

					PoolAble p=(PoolAble)e.next();

					// If it's not checked out, and it works, we have our man!
					if(p.isAvailable() && (p.isAlive())) {
						// Since we got one from the pool, we want to move it
						// to the end of the vector.
						poolable=p;
					}
				} // Flipping through the current pool

				// If we didn't get anything, and we're not at least
				// to our yellow line, open a new connection
				if(poolable==null && totalObjects()<yellowLine) {
					poolable=getNewObject();
				}

				// If we didn't get anything, deal with that situation.
				if(poolable==null) {

					try {
						debug("*** No free entries in pool, sleeping ***");

						// We're halfway through, or more!  Desperate measures!
						if(retry==retries/2) {
							debug("!!! Trying to force cleanup!");
							GarbageCollector gc=
								GarbageCollector.getGarbageCollector();
							gc.collect();
						}
						// Wait a half a second if the pool is full, in case
						// something gets checked in
						Thread.sleep(500);
					} catch(Exception e) {
						// Things just go faster.
					}
				}
			}// Retries for an object in the existing pool.

			// Check it out right now.
			if(poolable!=null) {
				rv=new PooledObject(poolable);
			}

		} // End of pool synchronization

		// If the above didn't get us an object, we'll resort to getting a
		// new one.
		if(rv==null) {
			// OK, got nothing from the pool, in a desperate attempt, we'll
			// be grabbing a new object.
			poolable=getNewObject();
			rv=new PooledObject(poolable);
		}

		// OK, let's stick it at the end of the vector (may already be, but
		// you know...) so that it's one of the last we check for next time.

		// Hold it still whlie we do this...
		synchronized(pool) {
			debug("Moving " + poolable);
			pool.remove(poolable);
			pool.add(poolable);
		}

		return(rv);
	}

	// Name to print in debuggy type things.
	private String debugName() {
		StringBuffer rv=new StringBuffer();
		rv.append(name);
		rv.append(" @");
		rv.append(Integer.toHexString(hashCode()));

		return(rv.toString());
	}

	/**
	 * debugging tool, dump out the current state of the pool
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer();
		sb.append("Pool ");
		sb.append(debugName());
		sb.append(" - total Objects:  ");
		sb.append(totalObjects());
		sb.append(", available objects:  ");
		sb.append(availableObjects());
		sb.append('\n');

		synchronized (pool) {
			for(Iterator i=pool.iterator(); i.hasNext();) {
				sb.append("    ");
				sb.append(i.next());
				sb.append("\n");
			}
		}
		return(sb.toString());
	}

	/**
	 * Find out how many objects are available in this pool.
	 *
	 * @return the number of available (not checked out) objects.
	 */
	public int availableObjects() {
		int ret=0;

		synchronized (pool) {
			for(Iterator i=pool.iterator(); i.hasNext();) {
				PoolAble p=(PoolAble)i.next();
				if(p.isAvailable()) {
					ret++;
				}
			}
		}

		return(ret);
	}

	/**
	 * Remove any object that is not checked out, as long as we stay above
	 * our minimum object requirement.
	 * <p>
	 * This method should only be called from the ObjectPoolCleaner --
	 * please don't call it directly.
	 *
	 * @exception PoolException when something bad happens
	 */
	public void prune() throws PoolException {
		debug("Beginning prune.");
		synchronized (pool) {
			int i=0;
			// Get rid of expired things
			for(Iterator it=pool.iterator(); it.hasNext();) {
				PoolAble p=(PoolAble)it.next();
				if(p.pruneStatus()>=PoolAble.MUST_CLEAN) {
					// Tell it that it can go away now.
					debug("Removing " + p);
					p.discard();
					it.remove();
				}
			}

			// If we don't have enough objects, go get more!  They're cheap!
			if(totalObjects()<minObjects) {
				getMinObjects();
			}
		} // pool lock
	}

	private void initialize() throws PoolException {
		pool=new ArrayList();

		// Get the min and max args.
		minObjects=getPropertyInt("min", 0);
		initObjects=getPropertyInt("start", minObjects);
		maxObjects=getPropertyInt("max", 5);
		// The yellow line is the number of connections before we start to
		// slow it down...
		yellowLine=(int)((float)maxObjects
			* (float)getPropertyInt("yellow_line", 75)/100.0);

		// Set the hashcode of this pool for consistent debug output.
		filler.setPoolHash(hashCode());

		debug("Pool " + debugName() + " wants a min of " + minObjects
			+ " and a max of " + maxObjects
			+ " with a yellow line at " + yellowLine);

		try {
			getStartObjects();
		} catch(PoolException e) {
			// If there was a problem initializing the pool, throw away
			// what we've got.
			for(Iterator i=pool.iterator(); i.hasNext(); ) {
				PoolAble p=(PoolAble)i.next();
				p.discard();
			}
			throw e;
		}
	}

	// Populate with the minimum number of objects.
	private void getMinObjects() throws PoolException{
		debug("Pool " + name + " wants at least " + minObjects +" objects.");
		for(int i=totalObjects(); i<minObjects; i++) {
			getNewObject();
		}
	}

	// Populate with the number of objects we need at start.
	private void getStartObjects() throws PoolException{
		debug("Pool " + name + " starting with " + initObjects +" objects.");
		for(int i=totalObjects(); i<initObjects; i++) {
			getNewObject();
		}
	}

	// Fetch a new object from the poolfiller, the pool is exhausted and we
	// need more objects.
	private PoolAble getNewObject() throws PoolException {
		PoolAble p=null;

		// First, if we're at capacity, do a prune and see if we can shrink
		// it down a bit.
		if(totalObjects()>=maxObjects) {
			prune();
		}

		// Don't add an object if we're at capacity.
		if(totalObjects()<maxObjects) {
			debug("*** Getting a new object in the "
				+ name + " pool, currently have " + totalObjects()
				+ "/" + maxObjects + ". ***");
			p=filler.getObject();
			p.setObjectID(nextId());
			p.setPoolName(name);
			// Calculate a lifetime and set it
			p.setMaxAge(calculateMaxAge());
			p.activate();
			synchronized(pool) {
				pool.add(p);
			}
			debug("Added the object to the pool, now have "
				+ totalObjects());
		} else {
			throw new PoolException("Cannot create another object in the pool");
		}
		return(p);
	}

	// Calculate the maximum age of the ``next'' object based on the number
	// of objects currently in the pool.  The more full the pool is, the
	// less time anything should stay in it.  This does nifty burst
	// compensation.
	private long calculateMaxAge() {
		// Default to whatever's in the config
		long rv=(long)getPropertyInt("max_age", 0);
		synchronized(pool) {
			int poolSize=totalObjects();
			// Only create a new maxAge if we're above our minimum threshold
			if(poolSize>minObjects) {
				float percentFull=(float)poolSize/(float)maxObjects;
				float factor=1-percentFull;
				rv=(long)((double)rv*factor);
				// All connections should be available for at least 5 seconds
				if(rv<5000) {
					rv=5000;
				}
			}
		}
		return(rv);
	}

	/**
	 * Find out how many objects are in this pool.  This will be the sum of
	 * the available and unavailable objects.
	 */
	public int totalObjects() {
		int ret=-1;
		synchronized(pool) {
			ret=pool.size();
		}
		return(ret);
	}

	private int getPropertyInt(String what, int def) {
		return(conf.getInt(name + "." + what, def));
	}

	private String getProperty(String what, String def) {
		return(conf.get(name + "." + what, def));
	}

	private String getProperty(String what) {
		return(conf.get(name + "." + what));
	}

	private static synchronized int nextId() {
		objectId++;
		return(objectId);
	}

	private void debug(String msg) {
		if(pooldebug==null) {
			pooldebug=new PoolDebug();
		}
		pooldebug.debug(msg);
	}

}
