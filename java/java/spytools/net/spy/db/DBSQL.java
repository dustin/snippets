// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: DBSQL.java,v 1.1 2001/03/27 09:30:52 dustin Exp $

package net.spy.db;

import java.util.*;
import java.util.Date; // Avoid ambiguous class with java.sql.Date
import java.sql.*;
import java.math.*;

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

		try {
			checkArgs();
		} catch (SQLException se) {
			String msg="Found an error when checking the arguments: " + se;
			throw new SQLException(msg);
		}
		applyArgs(required_inorder);
	}
}
