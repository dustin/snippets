//
// $Id: SNPPPoolFiller.java,v 1.1 2000/07/04 09:44:10 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.net.SNPP;
import net.spy.SpyConfig;

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

			long max_age=(long)getPropertyInt("max_age", 0);

			// Grab a connection.
			SNPP db = new SNPP(hostname, port);
			// Create the PoolAble object
			p=new SNPPPoolAble(db, max_age);
		} catch(Exception e) {
			throw new PoolException(
				"Error getting new SNPP object for the "
					+ name + " pool:  " + e
				);
		}

		return(p);
	}
}
