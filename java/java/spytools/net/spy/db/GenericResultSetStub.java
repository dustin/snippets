/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: GenericResultSetStub.java,v 1.4 2002/08/21 00:53:04 dustin Exp $
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
import java.util.List;
import java.util.ArrayList;

/**
 * This object contains all the common stuff required to stub a ResultSet
 * implementation.
 */
public abstract class GenericResultSetStub extends Object implements Cloneable {

	// Here is where the ResultSet data gets stored.
	private List results=null;
	private Iterator resultIter=null;

	// Map column names to ints
	private HashMap columns=null;

	// This is the current result we're looking at.
	private Object result[]=null;

	// We're going to keep a copy of the ResultSetMetaData from the
	// original query.  Let's find out if those things require an active
	// ResultSet, shall we?
	private ResultSetMetaData metadata=null;

	// Used for wasNull()
	private boolean wasNull=false;

	/**
	 * Magically transform the passed in ResultSet to a
	 * GenericResultSetStub
	 *
	 * @param rs the ResultSet we want to magically transform
	 *
	 * @exception SQLException if the ResultSet somehow fails us.
	 */
	public GenericResultSetStub(ResultSet rs) throws SQLException {
		super();
		setMetaData(rs.getMetaData());
		initResults(rs);
	}

	/**
	 * Get an instance of a GenericResultSetStub with no initialization.
	 *
	 * The subclass is expected to do whatever is necessary, and then call
	 * setResults(List) and setMetaData(ResultSetMetaData) to make
	 * everything happy.
	 *
	 * @throws SQLException (never)
	 */
	protected GenericResultSetStub() throws SQLException {
		super();
	}

	// result set initializer
	private void initResults(ResultSet rs) throws SQLException {
		// Initialize the results vector
		ArrayList r=new ArrayList();
		int ncolumns=metadata.getColumnCount();

		// Flip through each result
		while(rs.next()) {

			// Get an array to store them
			Object result[]=new Object[ncolumns];

			// Flip through the columns
			for(int i=1; i<=ncolumns; i++) {
				// Get the object from the result set.
				result[i-1]=rs.getObject(i);

				// If we did a numeric thingy
				if(rs.wasNull()) {
					result[i-1]=null;
				}
			} // columns

			// Stick it in our resultset.
			r.add(result);

		} // results

		setResults(r);
	} // initresults

	/**
	 * Reset the results to the beginning.
	 */
	protected void resetResults() {
		this.resultIter=null;
	}

	/**
	 * Set the results for this ResultSet to use.
	 *
	 * @param results results list to use
	 */
	protected void setResults(List results) {
		this.results=results;
		resetResults();
	}

	/**
	 * Set the ResultSetMetaData used for this ResultSet.
	 *
	 * @param rsmd the ResultSetMetaData
	 * @throws SQLException if there's a problem examining the signature
	 */
	protected void setMetaData(ResultSetMetaData rsmd) throws SQLException {
		this.metadata=rsmd;

		// Get the column count
		int ncolumns=metadata.getColumnCount();

		// Initialize columns
		columns=new HashMap();
		for(int i=1; i<=ncolumns; i++) {
			String name=metadata.getColumnName(i).toLowerCase();
			columns.put(name, new Integer(i));
		}
	}

	/**
	 * Get the Object at the given result column for the current result
	 * row.
	 *
	 * @param index the numeric index of the column
	 * @return whatever Object is found at that column (may be null)
	 * @throws SQLException if there is not a current ResultSet row, or the
	 * index is out of bounds
	 */
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
			wasNull=true;
		} else {
			wasNull=false;
		}

		// OK, return the column
		return(result[index]);
	}

	/**
	 * Debug routine for displaying the current row of the ResultSet.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer(256);
		sb.append("Result row:\n");
		int ncolumns=0;
		try {
			ncolumns=metadata.getColumnCount();
		} catch(SQLException e) {
			ncolumns=0;
		}
		for(int i=1; i<=ncolumns; i++) {
			try {
				Object o=getResultColumn(i);
				if(o!=null) {
					sb.append("\t");
					sb.append(metadata.getColumnName(i));
					sb.append("=");
					sb.append(o);
					sb.append(" (");
					sb.append(o.getClass().getName());
					sb.append(")\n");
				} else {
					sb.append("\t");
					sb.append(metadata.getColumnName(i));
					sb.append(" - null\n");
				}
			} catch(SQLException e) {
				// Ignore these columns, it's apparently broken
				e.printStackTrace();
			}
		}
		return(sb.toString());
	}

	// OK, begin disgustingly long stream of interface implementation

	/**
	 * Seek to the next row in this ResultSet.
	 *
	 * @return true if there was another row.
	 * @throws SQLException if there's a problem seeking...or something
	 */
	public boolean next() throws SQLException {
		boolean rv=true;
		// Make sure we've got our enumeration going.
		if(resultIter==null) {
			resultIter=results.iterator();
		}

		if(resultIter.hasNext()) {
			result=(Object[])resultIter.next();
		} else {
			rv=false;
		}

		return(rv);
	}

	/**
	 * Close the ResultSet.
	 *
	 * This currently does nothing.
	 *
	 * @throws SQLException (never)
	 */
	public void close() throws SQLException {
		// Nothing.
	}

	/**
	 * Check to see if the last column fetched was null.
	 *
	 * @return true if it was null
	 * @throws SQLException (never)
	 */
	public boolean wasNull() throws SQLException {
		return(wasNull);
	}

	/**
	 * Get the given column as a string.
	 *
	 * @param index the index of the column
	 * @return the String value of the column, or null
	 * @throws SQLException if misused
	 */
	public String getString(int index) throws SQLException {
		Object o=getResultColumn(index);
		if(o==null) {
			return(null);
		}
		return(o.toString());
	}

	/**
	 * Get the given value as a boolean.
	 *
	 * @param index the index of the column
	 * @return the boolean value of the column
	 * @throws SQLException if there's a problem getting the value
	 */
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

	/**
	 * @see ResultSet
	 */
	public short getShort(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.shortValue());
	}

	/**
	 * @see ResultSet
	 */
	public int getInt(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.intValue());
	}

	/**
	 * @see ResultSet
	 */
	public long getLong(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.longValue());
	}

	/**
	 * @see ResultSet
	 */
	public float getFloat(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.floatValue());
	}

	/**
	 * @see ResultSet
	 */
	public double getDouble(int index) throws SQLException {
		Number n=getNumber(index);
		return(n.doubleValue());
	}

	/**
	 * @see ResultSet
	 */
	private Number getNumber(int index) throws SQLException {
		Number n=null;
		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				n=(Number)o;
			} catch(Exception e) {
				throw new SQLException("Error getting Number value:  " + e);
			}
		} else {
			n=new Integer(0);
		}
		return(n);
	}

	/**
	 * @see ResultSet
	 */
	public BigDecimal getBigDecimal(int index) throws SQLException {
		BigDecimal rv=null;

		Object o=getResultColumn(index);
		if(o!=null) {
			try {
				rv=(BigDecimal)o;
			} catch(Exception e) {
				throw new SQLException("Error getting date value:  " + e);
			}
		}

		return(rv);
	}

	/**
	 * @see ResultSet
	 */
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

	/**
	 * @see ResultSet
	 */
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

	/**
	 * @see ResultSet
	 */
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

	/**
	 * @see ResultSet
	 */
	public Object getObject(int index) throws SQLException {
		return(getResultColumn(index));
	}

	//
	// OK, now we gotta do them all again, but this time, by name
	//

	/**
	 * @see ResultSet
	 */
	public int findColumn(String columnName) throws SQLException {
		String name=columnName.toLowerCase();
		Integer value=(Integer)columns.get(name);
		if(value==null) {
			throw new SQLException("No such column:  " + columnName);
		}
		return(value.intValue());
	}

	// OK, now it's basically cut, change types, paste

	/**
	 * @see ResultSet
	 */
	public String getString(String name) throws SQLException {
		return(getString(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public boolean getBoolean(String name) throws SQLException {
		return(getBoolean(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public short getShort(String name) throws SQLException {
		return(getShort(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public int getInt(String name) throws SQLException {
		return(getInt(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public long getLong(String name) throws SQLException {
		return(getLong(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public float getFloat(String name) throws SQLException {
		return(getFloat(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public double getDouble(String name) throws SQLException {
		return(getDouble(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public java.sql.Date getDate(String name) throws SQLException {
		return(getDate(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public java.sql.Time getTime(String name) throws SQLException {
		return(getTime(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public java.sql.Timestamp getTimestamp(String name) throws SQLException {
		return(getTimestamp(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public BigDecimal getBigDecimal(String name) throws SQLException {
		return(getBigDecimal(findColumn(name)));
	}

	/**
	 * @see ResultSet
	 */
	public Object getObject(String name) throws SQLException {
		return(getObject(findColumn(name)));
	}

	// OK, that sucked, on to more stuff we don't use right now...

	/**
	 * @see ResultSet
	 */
	public SQLWarning getWarnings() throws SQLException {
		return(null);
	}

	/**
	 * @see ResultSet
	 */
	public void clearWarnings() throws SQLException {
		// nothing
	}

	/**
	 * @see ResultSet
	 */
	public ResultSetMetaData getMetaData() throws SQLException {
		return(metadata);
	}
}
