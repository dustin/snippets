/*
 * Copyright (c) 2000  Dustin Sallings <dustin@beyond.com>
 *
 * $Id: CachedResultSetStub.java,v 1.4 2000/11/09 08:21:07 dustin Exp $
 */

package net.spy.db;

import java.util.*;
import java.math.*;
import java.io.*;
import java.sql.*;

/**
 * This object represents a cached java.sql.ResultSet.  It will hopefully
 * only contain small results.
 */

public class CachedResultSetStub extends Object implements Cloneable {
	// Here is where the ResultSet data gets stored.
	protected Vector results=null;
	protected Enumeration resultEnum=null;

	// Map column names to ints
	protected Hashtable columns=null;

	// This is the current result we're looking at.
	protected Object result[]=null;

	// We're going to keep a copy of the ResultSetMetaData from the
	// original query.  Let's find out if those things require an active
	// ResultSet, shall we?
	protected ResultSetMetaData metadata=null;

	// Used for wasNull()
	protected boolean _wasnull=false;

	/**
	 * Magically transform the passed in ResultSet to a CachedResultSet
	 *
	 * @param rs the ResultSet we want to magically transform
	 *
	 * @exception SQLException if the ResultSet somehow fails us.
	 */
	public CachedResultSetStub(ResultSet rs) throws SQLException {
		super();
		metadata=rs.getMetaData();
		initResults(rs);
	}

	/**
	 * Make a copy of this object.
	 */
	public CachedResultSetStub newCopy() {
		try {
			return((CachedResultSetStub)clone());
		} catch(Exception e) {
			// The exceptions this thing throws, well, aren't
		}
		return(null);
	}

	// Read in all of the results, and do our best to conver them to
	// something meaningful to us.
	protected void initResults(ResultSet rs) throws SQLException {
		// Initialize the results vector
		results=new Vector();
		int ncolumns=metadata.getColumnCount();

		// Initialize columns
		columns=new Hashtable();
		for(int i=1; i<=ncolumns; i++) {
			String name=metadata.getColumnName(i).toLowerCase();
			columns.put(name, new Integer(i));
		}

		// Flip through each result
		while(rs.next()) {

			// Get an array to store them
			Object result[]=new Object[ncolumns];

			// Flip through the columns
			for(int i=1; i<=ncolumns; i++) {
				String tmp=null;

				// Default to null
				result[i-1]=null;

				// Now, figure out what type of data this column is...
				switch(metadata.getColumnType(i)) {
					case Types.DOUBLE:
					case Types.REAL:
						result[i-1]=new Double(rs.getDouble(i));
						break;
					case Types.DECIMAL:
						tmp=rs.getString(i);
						if(tmp!=null) {
							result[i-1]=new BigDecimal(tmp);
						}
						break;
					case Types.NUMERIC:
					case Types.BIGINT:
						tmp=rs.getString(i);
						if(tmp!=null) {
							result[i-1]=new BigInteger(tmp);
						}
						break;
					case Types.FLOAT:
						result[i-1]=new Float(rs.getFloat(i));
						break;
					case Types.CHAR:
					case Types.VARCHAR:
					case Types.LONGVARCHAR:
						result[i-1]=rs.getString(i);
						break;
					case Types.DATE:
						result[i-1]=rs.getDate(i);
						break;
					case Types.TIME:
						result[i-1]=rs.getTime(i);
						break;
					case Types.TIMESTAMP:
						result[i-1]=rs.getTimestamp(i);
						break;
					case Types.SMALLINT:
					case Types.TINYINT:
					case Types.INTEGER:
						result[i-1]=new Long(rs.getLong(i));
						break;
					case Types.BIT:
						result[i-1]=new Boolean(rs.getBoolean(i));
						break;
					case Types.OTHER:
						result[i-1]=rs.getObject(i);
						break;
					case Types.NULL:
						result[i-1]=null;
						break;
					default:
						throw new SQLException("Unhandled data type:  "
							+ metadata.getColumnType(i));

				}

				// If we did a numeric thingy
				if(rs.wasNull()) {
					result[i-1]=null;
				}

			} // columns

			// Stick it in our resultset.
			results.addElement(result);

		} // results
	} // initresults

	protected Object getResultColumn(int index) throws SQLException {
		// (jdbc is offset at 1, we're offset at 0)
		index--;
		if(result==null) {
			throw new SQLException("No current result.");
		}
		if(index<0) {
			throw new SQLException("ResultSets start at one, nothing less.");
		}
		if(index>=result.length) {
			throw new SQLException("There are only "
				+ result.length + " columns in this set.");
		}

		// Check for NULL
		if(result[index]==null) {
			_wasnull=true;
		} else {
			_wasnull=false;
		}

		// OK, return the column
		return(result[index]);
	}

	/**
	 * Debug routine for displaying the current row of the ResultSet.
	 */
	public String toString() {
		String ret="Result row:\n";
		int ncolumns=0;
		try {
			ncolumns=metadata.getColumnCount();
		} catch(Exception e) {
			ncolumns=0;
		}
		for(int i=1; i<=ncolumns; i++) {
			try {
				Object o=getResultColumn(i);
				if(o!=null) {
					ret+="\t" + metadata.getColumnName(i) + "=" + o + "\n";
				}
			} catch(Exception e) {
				// Ignore this columns, it's apparently broken
			}
		}
		return(ret);
	}

	// OK, begin disgustingly long stream of interface implementation

	// ``next''
	public boolean next() throws SQLException {
		boolean rv=true;
		// Make sure we've got our enumeration going.
		if(resultEnum==null) {
			resultEnum=results.elements();
		}

		if(resultEnum.hasMoreElements()) {
			result=(Object[])resultEnum.nextElement();
		} else {
			rv=false;
		}

		return(rv);
	}

	public void close() throws SQLException {
		// Nothing.
	}

	public boolean wasNull() throws SQLException {
		return(_wasnull);
	}

	public String getString(int index) throws SQLException {
		Object o=getResultColumn(index);
		if(o==null) {
			return(null);
		}
		return(o.toString());
	}

	public boolean getBoolean(int index) throws SQLException {
		boolean rv=false;
		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				Boolean b=(Boolean)o;
				rv=b.booleanValue();
			} catch(Exception e) {
				throw new SQLException("Error getting boolean value:  " + e);
			}
		}

		return(rv);
	}

	public short getShort(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.shortValue());
	}

	public int getInt(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.intValue());
	}

	public long getLong(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.longValue());
	}

	public float getFloat(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.floatValue());
	}

	public double getDouble(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.doubleValue());
	}

	protected Number getNumber(int index) throws SQLException {
		Number n=null;
		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				n=(Number)o;
			} catch(Exception e) {
				throw new SQLException("Error getting Number value:  " + e);
			}
		}
		return(n);
	}

	public java.sql.Date getDate(int index) throws SQLException {
		java.sql.Date rv=null;
		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				rv=(java.sql.Date)o;
			} catch(Exception e) {
				throw new SQLException("Error getting date value:  " + e);
			}
		}
		return(rv);
	}

	public java.sql.Time getTime(int index) throws SQLException {
		java.sql.Time rv=null;
		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				rv=(java.sql.Time)o;
			} catch(Exception e) {
				throw new SQLException("Error getting time value:  " + e);
			}
		}
		return(rv);
	}

	public java.sql.Timestamp getTimestamp(int index) throws SQLException {
		java.sql.Timestamp rv=null;
		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				rv=(java.sql.Timestamp)o;
			} catch(Exception e) {
				throw new SQLException("Error getting timestamp value:  " + e);
			}
		}
		return(rv);
	}

	public Object getObject(int index) throws SQLException {
		return(getResultColumn(index));
	}

	//
	// OK, now we gotta do them all again, but this time, by name
	//

	// Let's start with findColumn so I can get that out of the way
	public int findColumn(String columnName) throws SQLException {
		String name=columnName.toLowerCase();
		Integer value=(Integer)columns.get(name);
		if(value==null) {
			throw new SQLException("No such column:  " + columnName);
		}
		return(value.intValue());
	}

	// OK, now it's basically cut, change types, paste

	public String getString(String name) throws SQLException {
		return(getString(findColumn(name)));
	}

	public boolean getBoolean(String name) throws SQLException {
		return(getBoolean(findColumn(name)));
	}

	public short getShort(String name) throws SQLException {
		return(getShort(findColumn(name)));
	}

	public int getInt(String name) throws SQLException {
		return(getInt(findColumn(name)));
	}

	public long getLong(String name) throws SQLException {
		return(getLong(findColumn(name)));
	}

	public float getFloat(String name) throws SQLException {
		return(getFloat(findColumn(name)));
	}

	public double getDouble(String name) throws SQLException {
		return(getDouble(findColumn(name)));
	}

	public java.sql.Date getDate(String name) throws SQLException {
		return(getDate(findColumn(name)));
	}

	public java.sql.Time getTime(String name) throws SQLException {
		return(getTime(findColumn(name)));
	}

	public java.sql.Timestamp getTimestamp(String name) throws SQLException {
		return(getTimestamp(findColumn(name)));
	}

	public Object getObject(String name) throws SQLException {
		return(getObject(findColumn(name)));
	}

	// OK, that sucked, on to more stuff we don't use right now...

	public SQLWarning getWarnings() throws SQLException {
		return(null);
	}

	public void clearWarnings() throws SQLException {
		// nothing
	}

	public ResultSetMetaData getMetaData() throws SQLException {
		return(metadata);
	}
}
