// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPool.java,v 1.8 2000/07/04 05:40:38 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

/**
 * ObjectPool is the entry point for all object pooling facilities in
 * net.spy.pool.*.  ObjectPools have a shared reference to a pool, so there
 * is exactly one set of pools per JVM.  This can be very useful in
 * consolidating applications' pools into one.
 * <p>
 * Pools are referenced by name, so as long as two pools have two different
 * names, they will be used independently.
 * <p>
 * When creating a pool, you must have a PoolFiller that will populate the
 * pool with objects when it needs them.
 * <p>
 * The following is an example demonstrating how to instantiate a JDBC pool
 * using JDBCPoolFiller:
 * <pre>
 * SpyConfig conf=new SpyConfig("pool.conf");
 * ObjectPool op=new ObjectPool(conf);
 * JDBCPoolFiller pf=new JDBCPoolFiller("db", conf);
 * op.createPool("db", pf);
 * </pre>
 */

public class ObjectPool extends Object {
	protected static Hashtable pools=null;
	protected Exception objectException=null;
	protected SpyConfig conf=null;
	protected ObjectPoolCleaner cleaner=null;

	public ObjectPool(SpyConfig conf) {
		super();
		this.conf=conf;

		initialize();
	}

	/**
	 * Create a new object pool.
	 *
	 * @param name The name of the object pool.
	 * @param pf The PoolFiller object that will be used to create new
	 * objects within the pool.
	 *
	 * @throws PoolException when bad things happen
	 */
	public void createPool(String name, PoolFiller pf)
		throws PoolException {

		synchronized(pools) {
			// Make sure we don't already have a this pool
			if(pools.containsKey(name)) {
				throw new PoolException("There's already a pool called "
					+ name);
			}

			// Grab a PoolContainer
			PoolContainer pc=new PoolContainer(name, pf, conf);

			// add it to our pool list
			pools.put(name, pc);
		}
	}

	/**
	 * Destory a pool.
	 *
	 * @param name The pool to destroy.
	 */
	public void destroyPool(String name) throws PoolException {
		synchronized (pools) {
			getPool(name);
			pools.remove(name);
		}
	}

	/**
	 * Get an object from a pool.
	 *
	 * @param name The pool from which we'll get our object.
	 *
	 * @return a PooledObject object.
	 *
	 * @throws PoolException if it can't get an object
	 */
	public PooledObject getObject(String name) throws PoolException {
		PooledObject ret=null;
		PoolContainer pc=null;
		synchronized (pools) {
			pc=getPool(name);
		}
		ret=pc.getObject();
		return(ret);
	}

	/**
	 * Dump out the object pools.
	 */
	public String toString() {
		String out="";
		Vector v=new Vector();
		synchronized (pools) {
			for(Enumeration e=pools.elements(); e.hasMoreElements(); ) {
				v.addElement(e.nextElement());
			}
		}
		// This is broken out to get out of the lock fast...
		for(Enumeration e=v.elements(); e.hasMoreElements(); ) {
			out+=e.nextElement();
		}
		return(out);
	}

	/**
	 * Prune the object pools.  This method requests that each individual
	 * pool prune itself, removing unusable or unnecessary PoolAbles.
	 */
	public void prune() throws PoolException {
		synchronized (pools) {
			for(Enumeration e=pools.elements(); e.hasMoreElements(); ) {
				PoolContainer pc=(PoolContainer)e.nextElement();
				pc.prune();
			}
		}
	}

	protected synchronized PoolContainer getPool(String name)
		throws PoolException {

		PoolContainer ret=null;

		synchronized (pools) {
			ret=(PoolContainer)pools.get(name);
			if(ret==null) {
				throw new PoolException("There's no pool called " + name);
			}
		}
		return(ret);
	}

	protected synchronized void initialize() {
		if(pools==null) {
			pools=new Hashtable();
			cleaner=new ObjectPoolCleaner(this);
		}
	}
}
