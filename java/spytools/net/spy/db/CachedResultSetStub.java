/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: CachedResultSetStub.java,v 1.14 2002/08/16 07:27:00 dustin Exp $
 */

package net.spy.db;

import java.math.BigDecimal;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Types;

import java.util.Iterator;
import java.util.HashMap;
import java.util.ArrayList;

/**
 * This object represents a cached java.sql.ResultSet.  It will hopefully
 * only contain small results.
 */
public class CachedResultSetStub extends GenericResultSetStub {

	/**
	 * Magically transform the passed in ResultSet to a CachedResultSet
	 *
	 * @param rs the ResultSet we want to magically transform
	 *
	 * @exception SQLException if the ResultSet somehow fails us.
	 */
	public CachedResultSetStub(ResultSet rs) throws SQLException {
		super(rs);
	}

	/**
	 * Make a copy of this object.
	 */
	public CachedResultSetStub newCopy() {
		try {
			return((CachedResultSetStub)clone());
		} catch(CloneNotSupportedException e) {
			// The exceptions this thing throws, well, aren't
			e.printStackTrace();
		}
		return(null);
	}
}
