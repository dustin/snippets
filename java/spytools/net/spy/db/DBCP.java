// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: DBCP.java,v 1.3 2002/08/27 20:54:44 dustin Exp $

package net.spy.db;

import java.sql.PreparedStatement;
import java.sql.CallableStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.sql.Connection;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.sql.Time;

import java.util.Map;
import java.util.Iterator;
import java.util.Collection;
import java.util.ArrayList;

import net.spy.SpyConfig;

/**
 * Superclass for dynamic SQL calls.
 */
public abstract class DBCP extends DBSP {

	private int argument_index=1;

	/**
	 * Get a DBCP object with the given DBConfig.
	 */
	public DBCP(SpyConfig conf) throws SQLException {
		super(conf);
	}

	/**
	 * Get a DBCP object with the given Connection.
	 */
	public DBCP(Connection conn) throws SQLException {
		super(conn);
	}

	/**
	 * Execute a query for update only.
	 */
	public boolean execute() throws SQLException  {
		boolean rv=true;
		prepare();
		rv=getPreparedStatement().execute();
		return(rv);
	}

	/**
	 * Optional parameters are not currently supported for DBCP objects.
	 * This method will always throw a SQLException when called.
	 *
	 * @param name name of the parameter
	 * @param type type of the parameter
	 * @throws SQLException Optional Parameters are not supported
	 */
	protected void setOptional(String name, int type) throws SQLException {
		throw new SQLException("Optional Parameters Not Supported on DBCP");
	}

	/**
	 * Get the CallableStatement for fetching output args.
	 * TODO get the get* methods implemented
	 *
	 * @return The CallableStatement for getting the RC's from.
	 */
	public CallableStatement getCallableStatement() {
		CallableStatement rc=(CallableStatement)getPreparedStatement();
		return(rc);
	}

	/**
	 * Fill in the arguments (with types) for the given list of parameters.
	 *
	 * @param query the query we'll be calling
	 * @param v the list of named parameters we need to add, in order
	 */
	protected void applyArgs(Collection v, boolean output_args)
			throws SQLException {

		PreparedStatement pst=getPreparedStatement();
		// Get the statement
		if (pst==null) {
			if(getCacheTime()>0) {
				throw new Error ("Not Implemented!!");
				//pst=prepareStatement(query, getCacheTime());
			} else {
				pst=prepareCall(getQuery());
			}
		}

		// Get the arguments and types
		Map args=getArgs();
		Map types=getTypes();

		// Use this iterator for the now positional arguments
		for(Iterator e=v.iterator(); e.hasNext(); ) {
			int i=argument_index;
			String key=(String)e.next();
			Object o=args.get(key);

			Integer typeInt=(Integer)types.get(key);

			// Check the type
			if(typeInt == null) {
				throw new SQLException ("No argument given for " + key);
			}

			// Get it as an int so we can switch it
			int type=typeInt.intValue();

			try {
				if(output_args) {
					if (isDebugEnabled()) {
						System.err.println("OUT -> Setting column "
							+key+"("+i+") type "+type);
					}
					CallableStatement cst=(CallableStatement)pst;
					cst.registerOutParameter(i, type);
				} else {
					if (isDebugEnabled()) {
						System.err.println("IN -> Setting column "
							+key+"("+i+") type "+type);
					}
					switch(type) {
						case Types.BIT:
							pst.setBoolean(i, ((Boolean)o).booleanValue());
							break;
						case Types.DATE:
							pst.setDate(i, (java.sql.Date)o);
							break;
						case Types.DOUBLE:
							pst.setDouble(i, ((Double)o).doubleValue());
							break;
						case Types.FLOAT:
							pst.setFloat(i, ((Float)o).floatValue());
							break;
						case Types.INTEGER:
							pst.setInt(i, ((Integer)o).intValue());
							break;
						case Types.BIGINT:
							pst.setLong(i, ((Long)o).longValue());
							break;
						case Types.NUMERIC:
						case Types.DECIMAL:
							BigDecimal bd=((BigDecimal)o).setScale(4,
									BigDecimal.ROUND_HALF_UP);
							pst.setBigDecimal(i, bd);
							break;
						case Types.SMALLINT:
						case Types.TINYINT:
							pst.setShort(i, (short)((Integer)o).intValue());
							break;
						case Types.NULL:
							pst.setNull(i, ((DBNull)o).getType());
							break;
						case Types.OTHER:
							pst.setObject(i, o);
							break;
						case Types.VARCHAR:
							pst.setString(i, (String)o);
							break;
						case Types.TIME:
							pst.setTime(i, (Time)o);
							break;
						case Types.TIMESTAMP:
							pst.setTimestamp(i, (Timestamp)o);
							break;
						default:
							throw new SQLException("Whoops, type "
								+ TypeNames.getTypeName(type) + "(" + type + ")"
								+ " seems to have been overlooked.");
					}
				} // end output_arg if statement
			} catch(SQLException se) {
				throw se;
			} catch (Exception applyException) {
				applyException.printStackTrace();
				String msg="Problem setting " + key
					+ " in prepared statement for type "
					+ TypeNames.getTypeName(type) + " "
					+ o.toString() + " : " + applyException;
				throw new SQLException (msg);
			}

			argument_index++;

		}
	}

	/**
	 * Prepare the statement for execution.
	 */
	protected void prepare() throws SQLException {

		// Make sure all the arguments are there.
		checkArgs();

		ArrayList alist=new ArrayList();

		// Get ready to build our query.
		StringBuffer querySb=new StringBuffer(256);
		querySb.append("{call ");
		querySb.append(getSPName());
		querySb.append(" (");

		// input vars
		for(Iterator e=getRequiredArgs().iterator(); e.hasNext(); ) {
			String param=(String)e.next();
			alist.add(param);
			querySb.append("?,");
		}

		// output vars
		for(Iterator e=getOutputArgs().iterator(); e.hasNext(); ) {
			String param=(String)e.next();
			alist.add(param);
			querySb.append("?,");
		}

		// Remove the last comma if we had params
		if(alist.size()>0) {
			querySb=new StringBuffer(querySb.toString().substring(0,
										querySb.length()-1));
		}

		// finish out
		querySb.append(")}");
		String query=querySb.toString().trim();

		PreparedStatement pst=getPreparedStatement();

		// Get a prepared statement, varies whether it's cachable or not.
		if (pst==null) {
			if(getCacheTime()>0) {
				throw new Error("Not Implemented");
				//pst=prepareCall(query, getCacheTime());
			} else {
				pst=prepareCall(query);
			}
		}

		// Fill in the arguments.
		setQuery(query);
		applyArgs(getRequiredArgs(), false);
		applyArgs(getOutputArgs(), true);
	}

}
