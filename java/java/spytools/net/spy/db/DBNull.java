// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: DBNull.java,v 1.2 2002/07/10 04:25:16 dustin Exp $

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


