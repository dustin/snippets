// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: JNDIConnectionSource.java,v 1.2 2002/08/23 00:13:16 dustin Exp $

package net.spy.db;

import java.util.Hashtable;

import java.sql.Connection;
import java.sql.SQLException;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import javax.sql.DataSource;

import net.spy.SpyConfig;

/**
 * Connection source for getting DB connections from JNDI sources.
 */
public class JNDIConnectionSource extends Object implements ConnectionSource {

	/**
	 * Get an instance of JNDIConnectionSource.
	 */
	public JNDIConnectionSource() {
		super();
	}

	/**
	 * This will get a connection to the JNDI resource.
	 *
	 * @param conf  The configuration to use for getting the database
	 * source
	 * @return A connection to the JNDI datasource
	 * @throws SQLException
	 * @see ConnectionSource
	 */
	public Connection getConnection(SpyConfig conf) throws SQLException {
		String source=conf.get("dbSource");

		Connection conn=null;
		try {
			// we need to convert the SpyConfig to a Hashtable since
			// the receiving end may not know what a SpyConfig is, but it
			// should surely know what a Hashtable is as that's what it's
			// expecting per the javadocs.
			Hashtable env=new Hashtable(conf);

			Context initial = new InitialContext(env);
			DataSource dsrc = (DataSource)initial.lookup(source);
			conn = dsrc.getConnection();
		} catch(NamingException e) {
			e.printStackTrace();
			throw new SQLException("Error getting connection from JNDI:  " + e);
		}

		return(conn);
	}

	/**
	 * @see ConnectionSource
	 */
	public void returnConnection(Connection conn) throws SQLException {
		conn.close();
	}

}
