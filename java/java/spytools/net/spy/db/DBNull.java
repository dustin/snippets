// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: DBNull.java,v 1.1 2001/03/27 09:30:50 dustin Exp $

package net.spy.db;

/**
 * Represents NULL data in DB parameters and stuff.
 */
public class DBNull extends Object {
	private int type=-1;

	/**
	 * Get a new null object.
	 */
	public DBNull(int type) {
		super();
		this.type=type;
	}

	/**
	 * Get the data type of this nullness.
	 */
	public int getType() {
		return type;
	}
}

