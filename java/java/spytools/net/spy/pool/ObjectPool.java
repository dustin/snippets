// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPool.java,v 1.2 2000/07/01 09:44:28 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

public class ObjectPool extends Object {
	protected static Hashtable pools=null;
	protected Exception objectException=null;
	protected SpyConfig conf=null;

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
	public synchronized void createPool(String name, PoolFiller pf)
		throws PoolException {

		// Make sure we don't already have a this pool
		if(pools.containsKey(name)) {
			throw new PoolException("There's already a pool called " + name);
		}

		// Grab a PoolContainer
		PoolContainer pc=new PoolContainer(name, pf, conf);

		// add it to our pool list
		pools.put(name, pc);
	}

	/**
	 * Destory a pool.
	 *
	 * @param name The pool to destroy.
	 */
	public synchronized void destroyPool(String name) throws PoolException {
		if(!pools.containsKey(name)) {
			throw new PoolException("There's no pool called " + name);
		}
		pools.remove(name);
	}

	/**
	 * Get an object from a pool.
	 *
	 * @param name The pool from which we'll get our object.
	 *
	 * @return a PoolAble object.
	 *
	 * @throws PoolException if it can't get an object
	 */
	public PoolAble getObject(String name) throws PoolException {
		PoolContainer pc=(PoolContainer)pools.get(name);
		if(pc==null) {
			throw new PoolException("There's no pool called " + name);
		}

		return(pc.getObject());
	}

	/**
	 * Dump out the object pools.
	 */
	public void dumpPools() {
		for(Enumeration e=pools.keys(); e.hasMoreElements(); ) {
			String key=(String)e.nextElement();

			System.out.println("Pool " + key);
			PoolContainer pc=(PoolContainer)pools.get(key);
			pc.dumpPool();
		}
	}

	protected synchronized void initialize() {
		if(pools==null) {
			pools=new Hashtable();
		}
	}
}
