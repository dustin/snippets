// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: DBSQL.java,v 1.5 2002/08/26 05:39:59 dustin Exp $

package net.spy.db;

import java.sql.Connection;
import java.sql.SQLException;

import net.spy.SpyConfig;

/**
 * Superclass for dynamic SQL calls.
 */
public abstract class DBSQL extends DBSP {

	/**
	 * Get a DBSQL object with the given DBConfig.
	 */
	public DBSQL(SpyConfig conf) throws SQLException {
		super(conf);
	}

	/**
	 * Get a DBSQL object with the given Connection.
	 */
	public DBSQL(Connection conn) throws SQLException {
		super(conn);
	}

	/**
	 * Prepare the SQL for execution.
	 */
	protected void prepare() throws SQLException {
		// Make sure all the arguments are there.

		checkArgs();
		applyArgs(getRequiredArgs());
	}
}
