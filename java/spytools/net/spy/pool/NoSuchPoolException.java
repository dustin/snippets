// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: NoSuchPoolException.java,v 1.1 2002/08/04 01:08:05 dustin Exp $

package net.spy.pool;

/**
 * Exception thrown when there's NoSuchPool.
 */
public class NoSuchPoolException extends PoolException {

	private String poolName=null;

	/**
	 * Get an instance of NoSuchPoolException.
	 */
	public NoSuchPoolException(String poolName) {
		super("There's no pool called " + poolName);

		this.poolName=poolName;
	}

	/**
	 * Get the name of the pool that is missing.
	 */
	public String getPoolName() {
		return(poolName);
	}

}
