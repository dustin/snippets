//
// $Id: PoolException.java,v 1.3 2002/08/04 01:08:06 dustin Exp $

package net.spy.pool;

/**
 * Exception thrown when there's a problem dealing with the pool.
 */
public class PoolException extends Exception {
	/**
	 * Get a PoolException instance.
	 */
	public PoolException(String msg) {
		super(msg);
	}
}

