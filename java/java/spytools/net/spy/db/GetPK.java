// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: GetPK.java,v 1.2 2002/08/24 07:23:00 dustin Exp $

package net.spy.db;

import java.util.HashMap;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Connection;

import java.math.BigDecimal;

import net.spy.SpyConfig;
import net.spy.SpyDB;
import net.spy.db.sp.UpdatePrimaryKey;
import net.spy.db.sp.SelectPrimaryKey;

/**
 * Primary key generator.
 */
public class GetPK extends Object {

	private static GetPK instance=null;

	private HashMap caches=null;

	/**
	 * Constructor for an extensible Singleton.
	 */
	protected GetPK() {
		super();
		caches=new HashMap();
	}

	/**
	 * Get the instance of GetPK.
	 *
	 * @return the instance
	 */
	public synchronized static GetPK getInstance() {
		if(instance==null) {
			instance=new GetPK();
		}
		return(instance);
	}

	/**
	 * Get a primary key from the database described in the given config.
	 *
	 * @param conf the configuration
	 * @param table the table for which the key is needed
	 * @return the key
	 * @throws SQLException if there's a problem getting the key
	 */
	public BigDecimal getPrimaryKey(SpyConfig conf, String table)
		throws SQLException {

		SpyDB db=new SpyDB(conf);
		BigDecimal pk=getPrimaryKey(db, table);
		db.close();
		return(pk);
	}

	// Get the key (usually from the cache)
	private BigDecimal getPrimaryKey(SpyDB db, String table)
		throws SQLException {

		BigDecimal rv=null;
		String key=table;
		try {
			KeyStore ks=(KeyStore)caches.get(key);
			// If we didn't get the key store, go get it now
			if(ks == null) {
				getKeysFromDB(db, table);
				ks=(KeyStore)caches.get(key);
				if(ks==null) {
					throw new SQLException("Couldn't get initial keys for "
						+ table);
				}
			}
			rv=ks.nextKey();
		} catch(OverDrawnException ode) {
			// Overdrawn, need to fetch the cache.
			getKeysFromDB(db, table);
			// Get the new key store
			KeyStore ks=(KeyStore)caches.get(key);
			try {
				rv=ks.nextKey();
			} catch(OverDrawnException ode2) {
				throw new Error("Primary keys not available after load.");
			}
		}

		return(rv);
	}

	/**
	 * Get the DBSP required for updating the primary key table.
	 *
	 * A subclass may override this to change the behavior of the first
	 * part of the ``fetch from db'' stage.  The DBSP returned will take
	 * exactly one parameter:  <code>table_name</code> and will be called
	 * via executeUpdate.  The update must update exactly <i>one</i> row.
	 * Any more or fewer will cause the process to fail and an exception
	 * will be thrown.
	 *
	 * <p/>
	 *
	 * For an example implementation, please see {@link UpdatePrimaryKey}.
	 *
	 * @param conn the connection to use (already in a transaction)
	 * @return the required DBSP
	 * @throws SQLException if there's a problem getting the DBSP
	 */
	protected DBSP getUpdateDBSP(Connection conn) throws SQLException {
		return(new UpdatePrimaryKey(conn));
	}

	/**
	 * Get the DBSP required for selecting primary key information back out
	 * of the primary key table.
	 *
	 * A subclass may override this method to change the behavior of the
	 * select statement that finds the range of results for a table.  The
	 * DBSP returned will take exactly one parameter:
	 * <code>table_name</code> and return a result set containing at least
	 * the following two columns:
	 *
	 * <ul>
	 *  <li><code>first_key</code> - the first key in the range</li>
	 *  <li><code>last_key</code> - the last key in the range</li>
	 * </ul>
	 *
	 * The ResultSet must contain exactly <i>one</i> row.  Any more or
	 * fewer will cause the process to fail and an exception will be
	 * thrown.
	 *
	 * <p/>
	 *
	 * For an example implementation, please see {@link SelectPrimaryKey}.
	 *
	 * @param conn the connection to use (already in a transaction)
	 * @return the required DBSP
	 * @throws SQLException if there's a problem getting the DBSP
	 */
	protected DBSP getSelectDBSP(Connection conn) throws SQLException {
		return(new SelectPrimaryKey(conn));
	}

	// get keys from a database
	private void getKeysFromDB(SpyDB db, String table) throws SQLException {
		Connection conn=null;
		boolean complete=false;

		try {
			conn=db.getConn();
			conn.setAutoCommit(false);

			// Update the table first
			DBSP dbsp=getUpdateDBSP(conn);
			dbsp.set("table_name", table);
			int changed=dbsp.executeUpdate();
			// Make sure one row got updated
			if(changed!=1) {
				throw new SQLException(
					"Did not update the correct number of rows");
			}
			dbsp.close();

			// Now, fetch it again
			dbsp=getSelectDBSP(conn);
			dbsp.set("table_name", table);
			ResultSet rs=dbsp.executeQuery();
			if(!rs.next()) {
				throw new SQLException("No results returned for primary key");
			}
			// Get the beginning and ending of the range
			BigDecimal start=rs.getBigDecimal("first_key");
			BigDecimal end=rs.getBigDecimal("last_key");
			if(rs.next()) {
				throw new SQLException(
					"Too many results returned for primary key");
			}
			// clean up
			rs.close();
			dbsp.close();

			synchronized(caches) {
				caches.put(table, new KeyStore(start, end));
			}

			complete=true;

		} finally {
			if(conn!=null) {
				if(complete) {
					try {
						conn.commit();
					} catch(SQLException e) {
						e.printStackTrace();
					}
				} else {
					try {
						conn.rollback();
					} catch(SQLException e) {
						e.printStackTrace();
					}
				}

				// Set autocommit back
				try {
					conn.setAutoCommit(true);
				} catch(SQLException e) {
					e.printStackTrace();
				}
			} // got a connection
		} // finally block
	} // getKeysFromDB

}
