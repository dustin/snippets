// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: JNDIConnectionSource.java,v 1.1 2002/08/03 06:07:05 dustin Exp $

package net.spy.db;

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
	 * @see ConnectionSource
	 */
	public Connection getConnection(SpyConfig conf) throws SQLException {
		String source=conf.get("dbSource");

		Connection conn=null;
		try {
			Context initial = new InitialContext();
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
		// XXX  I suppose there's something I should use to return this
		// stuff.
	}

}
