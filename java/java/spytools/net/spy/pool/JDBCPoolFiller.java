//
// $Id: JDBCPoolFiller.java,v 1.3 2000/07/04 05:40:37 dustin Exp $

package net.spy.pool;

import java.util.*;
import java.sql.*;
import net.spy.SpyConfig;

/**
 * PoolFiller object to fill a pool with JDBC PoolAbles
 */

public class JDBCPoolFiller extends PoolFiller {

	public JDBCPoolFiller(String name, SpyConfig conf) {
		super(name, conf);
	}

	/**
	 * get a new object for the pool.
	 *
	 * The following config entries are required:
	 * <ul>
	 *  <li>dbDriverName - Name of the JDBC driver to use</li>
	 *  <li>dbSource - JDBC url for the database</li>
	 *  <li>dbUser - Database username</li>
	 *  <li>dbPass - Database password</li>
	 * </ul>
	 *
	 * The following config entries are optional:
	 * <ul>
	 *  <li>max_age - The maximum amount of time (in milliseconds) that the
	 *      connection can live.  Default is forever</li>
	 * </ul>
	 *
	 * @throws PoolException if a new connection could not be made.
	 */
	public PoolAble getObject() throws PoolException {
		JDBCPoolAble p = null;
		try {
			String classname=null, source=null, user=null, pass=null;

			// Load the JDBC driver
			classname=getProperty("dbDriverName");
			if(classname==null) {
				throw new Exception("No dbDriverName property given");
			}
			Class.forName(classname);

			source=getProperty("dbSource");
			if(source==null) {
				throw new Exception("No dbSource property given");
			}

			user=getProperty("dbUser", "");
			pass=getProperty("dbPass", "");

			long max_age=(long)getPropertyInt("max_age", 0);

			// Grab a connection.
			Connection db = DriverManager.getConnection(source, user, pass);
			// Create the PoolAble object
			p=new JDBCPoolAble(db, max_age);
		} catch(Exception e) {
			throw new PoolException(
				"Error getting new DB object for the "
					+ name + " pool:  " + e
				);
		}

		return(p);
	}
}
