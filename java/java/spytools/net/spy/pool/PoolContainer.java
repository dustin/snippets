//
// $Id: PoolContainer.java,v 1.20 2001/03/17 21:58:54 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

import net.spy.SpyConfig;

/**
 * PoolContainer is the storage for a given pool.
 */
public class PoolContainer extends Object {
	private Vector pool=null;
	private SpyConfig conf=null;
	private String name=null;
	private PoolFiller filler=null;
	private int _min_objects=-1;
	private int _max_objects=-1;

	private static int _object_id=0;

	private boolean _debug=false;

	/**
	 * Create a new PoolContainer for a pool with a given name, and filler.
	 *
	 * The following optional config parameters will be used:
	 * <ul>
	 *  <li>&lt;poolname&gt;.min - minimum number of items in the pool</li>
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
		PoolAble ret=null;

		// How many times we're flipping through the object pool
		int retries=6;

		// Synchronize on the pool object.
		synchronized(pool) {
			// We'll try up to three seconds to get an object from the pool
			for(int retry=0; ret==null && retry<retries; retry++) {

				// Find the next available object.
				for(Enumeration e=pool.elements();
					ret==null && e.hasMoreElements(); ) {

					PoolAble p=(PoolAble)e.nextElement();

					// If it's not checked out, and it works, we have our man!
					if(p.isAvailable() && (p.isAlive())) {
						// Since we got one from the pool, we want to move it
						// to the end of the vector.
						ret=p;
					}
				} // Flipping through the current pool

				// If we didn't get anything, and we're not at least
				// half-way through our max object size, open a new
				// connection
				if(ret==null && totalObjects()<(_max_objects/2) ) {
					ret=getNewObject();
				}

				// If we didn't get anything, deal with that situation.
				if(ret==null) {

					try {
						debug("*** No free entries in pool, sleeping ***");

						// We're halfway through, or more!  Desperate measures!
						if(retry==retries/2) {
							debug("!!! Trying to force cleanup!");
							System.gc();
							System.runFinalization();
						}
						// Wait a half a second if the pool is full, in case
						// something gets checked in
						Thread.sleep(500);
					} catch(Exception e) {
						// Things just go faster.
					}
				}
			}// Retries for an object in the existing pool.
		} // End of pool synchronization

		// If the above didn't get us an object, we'll resort to getting a
		// new one.
		if(ret==null) {
			// OK, got nothing from the pool, in a desperate attempt, we'll
			// be grabbing a new object.
			ret=getNewObject();
		}

		// OK, let's stick it at the end of the vector (may already be, but
		// you know...) so that it's one of the last we check for next time.

		// Hold it still whlie we do this...
		synchronized(pool) {
			debug("Moving " + ret);
			pool.removeElement(ret);
			pool.addElement(ret);
		}

		// We're going to return a brand new PooledObject representing this
		// checked out entry.
		return(new PooledObject(ret));
	}

	/**
	 * debugging tool, dump out the current state of the pool
	 */
	public String toString() {
		String out="Pool " + name + "\n";
		synchronized (pool) {
			for(Enumeration e=pool.elements(); e.hasMoreElements();) {
				out+="    " + e.nextElement() + "\n";
			}
		}
		return(out);
	}

	/**
	 * Find out how many objects are available in this pool.
	 *
	 * @return the number of available (not checked out) objects.
	 */
	public int avaliableObjects() {
		int ret=0;

		synchronized (pool) {
			for(Enumeration e=pool.elements(); e.hasMoreElements();) {
				PoolAble p=(PoolAble)e.nextElement();
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
			// We're going to flip through this twice...once to remove
			// things that we *have* to, then once to remove things that we
			// can.
			for(Enumeration e=pool.elements(); e.hasMoreElements();) {
				PoolAble p=(PoolAble)e.nextElement();
				if(p.pruneStatus()==2) {
					// Tell it that it can go away now.
					debug("Removing " + p);
					p.discard();
					pool.removeElement(p);
				}
			}
			// OK, now let's get rid of the ones we can.
			for(Enumeration e=pool.elements(); e.hasMoreElements();) {
				PoolAble p=(PoolAble)e.nextElement();
				if(p.pruneStatus()==1) {
					if(totalObjects()>_min_objects) {
						// Tell it that it can go away now.
						debug("Removing " + p);
						p.discard();
						pool.removeElement(p);
					}
				}
			} // Getting rid of stuff

			// If we don't have enough objects, go get more!  They're cheap!
			if(totalObjects()<_min_objects) {
				getMinObjects();
			}
		} // pool lock
	}

	private void initialize() throws PoolException {
		pool=new Vector();

		// Get the min and max args.
		_min_objects=getPropertyInt("min", 0);
		_max_objects=getPropertyInt("max", 5);

		getMinObjects();
	}

	// Populate with the minimum number of objects.
	private void getMinObjects() throws PoolException{
		debug("Pool " + name + " wants at least " + _min_objects +" objects.");
		for(int i=totalObjects(); i<_min_objects; i++) {
			getNewObject();
		}
	}

	// Fetch a new object from the poolfiller, the pool is exhausted and we
	// need more objects.
	private PoolAble getNewObject() throws PoolException {
		PoolAble p=null;

		// First, if we're at capacity, do a prune and see if we can shrink
		// it down a bit.
		if(totalObjects()>=_max_objects) {
			prune();
		}

		// Don't add an object if we're at capacity.
		if(totalObjects()<_max_objects) {
			debug("*** Getting a new object in the "
				+ name + " pool, currently have " + totalObjects()
				+ ". ***");
			p=filler.getObject();
			p.setObjectID(nextId());
			p.setPoolName(name);
			p.activate();
			synchronized(pool) {
				pool.addElement(p);
			}
			debug("Added the object to the pool, now have "
				+ totalObjects());
		} else {
			throw new PoolException("Cannot create another object in the pool");
		}
		return(p);
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
		_object_id++;
		return(_object_id);
	}

	private void debug(String msg) {
		if(_debug) {
			System.out.println(msg);
		}
	}
}
