// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: GetPK.java,v 1.1 2002/08/23 07:46:21 dustin Exp $

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

	private static final int DEFAULT_NUM_KEYS=10;

	private HashMap caches=null;

	// singleton
	private GetPK() {
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

	// get keys from a database
	private void getKeysFromDB(SpyDB db, String table) throws SQLException {
		int num_keys=DEFAULT_NUM_KEYS;

		Connection conn=null;
		boolean complete=false;

		try {
			conn=db.getConn();
			conn.setAutoCommit(false);

			// Update the table first
			DBSP dbsp=new UpdatePrimaryKey(conn);
			dbsp.set("incr_amount", num_keys);
			dbsp.set("table_name", table);
			int changed=dbsp.executeUpdate();
			// Make sure one row got updated
			if(changed!=1) {
				throw new SQLException(
					"Did not update the correct number of rows");
			}
			dbsp.close();

			// Now, fetch it again
			dbsp=new SelectPrimaryKey(conn);
			dbsp.set("table_name", table);
			ResultSet rs=dbsp.executeQuery();
			if(!rs.next()) {
				throw new SQLException("No results returned for primary key");
			}
			// Get the end
			BigDecimal end=rs.getBigDecimal("primary_key");
			if(rs.next()) {
				throw new SQLException(
					"Too many results returned for primary key");
			}
			rs.close();
			dbsp.close();

			// OK, calculate the beginning
			BigDecimal start=end.subtract(new BigDecimal(num_keys-1));

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
