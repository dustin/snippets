//
// $Id: SNPPPoolFiller.java,v 1.5 2002/07/10 05:42:08 dustin Exp $

package net.spy.pool;

import net.spy.SpyConfig;

import net.spy.net.SNPP;

/**
 * PoolFiller object to fill a pool with SNPP PoolAbles
 */
public class SNPPPoolFiller extends PoolFiller {

	public SNPPPoolFiller(String name, SpyConfig conf) {
		super(name, conf);
	}

	/**
	 * get a new object for the pool.
	 *
	 * The following config entries are required:
	 * <ul>
	 *  <li>snppHost - SNPP server hostname</li>
	 * </ul>
	 *
	 * The following config entries are optional:
	 * <ul>
	 *  <li>snppPort - Alternate SNPP server port.  Default is 444</li>
	 *  <li>max_age - The maximum amount of time (in milliseconds) that the
	 *      connection can live.  Default is forever</li>
	 * </ul>
	 *
	 * @exception PoolException if a new connection could not be made.
	 */
	public PoolAble getObject() throws PoolException {
		SNPPPoolAble p = null;
		try {
			String hostname=null;
			int port=444;

			hostname=getProperty("snppHost");
			if(hostname==null) {
				throw new Exception("No snppHost property given");
			}

			port=getPropertyInt("snppPort", 444);

			int timeout=getPropertyInt("snppTimeout", 0);

			long maxAge=(long)getPropertyInt("max_age", 0);

			// Grab a connection.
			SNPP snpp = new SNPP(hostname, port, timeout);
			// Create the PoolAble object
			p=new SNPPPoolAble(snpp, maxAge, getPoolHash());
		} catch(Exception e) {
			throw new PoolException(
				"Error getting new SNPP object for the "
					+ debugName() + " pool:  " + e
				);
		}

		return(p);
	}
}
