// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: ConnectionSource.java,v 1.1 2002/08/02 22:00:13 dustin Exp $

package net.spy.db;

import java.sql.Connection;

import net.spy.SpyConfig;

/**
 * Interface for classes that will create database connections.
 */
public interface ConnectionSource {

	/**
	 * Get a new connection.
	 *
	 * @param conf a SpyConfig describing the connection that needs to be
	 * fetched.
	 *
	 * @exception SQLException if a connection could not be obtained
	 */
	public Connection getConnection(SpyConfig conf) throws SQLException;

}
