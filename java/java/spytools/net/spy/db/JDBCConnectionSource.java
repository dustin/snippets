// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: JDBCConnectionSource.java,v 1.1 2002/08/03 01:05:07 dustin Exp $

package net.spy.db;

import java.sql.Connection;

import net.spy.SpyConfig;

/**
 * Implementation of ConnectionSource that gets connections directly from
 * JDBC.
 */
public class JDBCConnectionSource implements ConnectionSource {

	/**
	 * Get a new connection from JDBC.
	 *
	 * <p>
	 *
	 * The configuration is expected to have the following values:
	 *
	 * <ul>
	 *  <li>dbDriverName - The database driver name</li>
	 *  <li>dbSource - The JDBC URL</li>
	 *  <li>dbUser - The database username</li>
	 *  <li>dbPass - The database password</li>
	 * </ul>
	 *
	 * </p>
	 *
	 * @param conf a SpyConfig describing the connection that needs to be
	 * fetched.
	 *
	 * @exception SQLException if a connection could not be obtained
	 */
	public Connection getConnection(SpyConfig conf) throws SQLException {
		SpyConfig c=new SpyConfig();
		c.put("user", conf.get("dbUser"));
		c.put("password", conf.get("dbPass"));

		String driver=conf.get("dbDriverName");
		String source=conf.get("dbSource");

		try {
			Class.forName(driver);
		} catch(ClassNotFoundException cnfe) {
			throw new SQLException("Couldn't load DB driver:  " + cnfe);
		}

		// Get the new connection
		Connection conn=DriverManager.getConnection(source, c);

		// Return it
		return(conn);
	}

}
