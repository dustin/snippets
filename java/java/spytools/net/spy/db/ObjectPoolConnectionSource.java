// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPoolConnectionSource.java,v 1.2 2002/08/03 06:43:25 dustin Exp $

package net.spy.db;

import java.util.Enumeration;

import java.sql.Connection;
import java.sql.SQLException;

import net.spy.SpyConfig;

import net.spy.pool.ObjectPool;
import net.spy.pool.PooledObject;
import net.spy.pool.PoolException;
import net.spy.pool.JDBCPoolFiller;

/**
 * Connection source to retrieve connections from an ObjectPool.
 */
public class ObjectPoolConnectionSource extends Object
	implements ConnectionSource {

	// Initialization flag.
	private static boolean initialized=false;

	// This is the object pool from which connections will be retrieved
	private static ObjectPool pool=null;

	// This is the pooled object that will be held until the connection is
	// finished.
	private PooledObject object=null;

	// The name of the pool referenced by this instance.
	private String poolName=null;

	/**
	 * Get an instance of SpyPoolConnectionSource.
	 */
	public ObjectPoolConnectionSource() {
		super();
	}

	/**
	 * @see ConnectionSource
	 */
	public Connection getConnection(SpyConfig conf) throws SQLException {
		// Get the pool name
		poolName=conf.get("dbPoolName");

		// make sure it's initialized
		if(!initialized) {
			initialize(conf);
		}

		Connection conn=null;
		try {
			// Snatch the pebble from my hand.
			object=pool.getObject(poolName);
			conn=(Connection)object.getObject();
		} catch(PoolException pe) {
			pe.printStackTrace();
			throw new SQLException("Could not get a DB connection:  " + pe);
		}
		return(conn);
	}

	/**
	 * @see ConnectionSource
	 */
	public void returnConnection(Connection conn) throws SQLException {
		object.checkIn();
		object=null;
	}

	// Perform one-time initialization for a config.
	private void initialize(SpyConfig gConf)
		throws SQLException {

		synchronized(ObjectPoolConnectionSource.class) {
			if(!initialized) {
				// Get a copy of the config, we'll be mangling it a bit
				SpyConfig conf = (SpyConfig)gConf.clone();
				if(pool==null) {
					pool=new ObjectPool(conf);
				}
				if(!pool.hasPool(poolName)) {
					try {
						createPool(conf);
					} catch(PoolException pe) {
						pe.printStackTrace();
						throw new SQLException("Error initializing pool:  "
							+ pe);
					}
				}
				initialized=true;
			}
		}
	}

	// Create the pool described in this config.
	private void createPool(SpyConfig conf)
		throws SQLException, PoolException {

		// get the normalized config
		SpyConfig normConf=getNormalizedConfig(poolName, conf);

		// Get the pool filler.
		JDBCPoolFiller pf=new JDBCPoolFiller(poolName, normConf);

		// Create the pool.
		pool.createPool(poolName, pf);
	}

	// Get a config with all of the expected values for a DB pool
	private SpyConfig getNormalizedConfig(String name, SpyConfig conf)
		throws SQLException {

		// Will return a new instance of SpyConfig.
		SpyConfig rv=new SpyConfig();

		// If a name is given, all properties should be prefixed with that name
		String prefix="";
		if(name!=null) {
			prefix=name+".";
		}

		// Minimum connections in the pool.
		int minConns=conf.getInt("dbMinConns", 1);
		rv.put(prefix + "min", "" + minConns);

		// Start connections
		int startConns=conf.getInt("dbStartCons", minConns);
		rv.put(prefix + "start", "" + startConns);

		// Yellow line percentage
		int yellowLine=conf.getInt("dbYellowLine", -1);
		if(yellowLine>0) {
			rv.put(prefix + "yellow_line", "" + yellowLine);
		}

		// Maximum number of connections in the pool.
		int maxConns=conf.getInt("dbMaxConns", 5);
		rv.put(prefix + "max", "" + maxConns);

		// Maximum age of any item in the pool
		String tmp=conf.get("dbMaxLifeTime");
		if(tmp==null) {
			tmp="86400";
		}
		rv.put(prefix+"max_age", tmp);

		// Driver name
		tmp=conf.get("dbDriverName");
		if(tmp==null) {
			throw new SQLException(
				"dbDriverName not given, invalid configuration.");
		}
		rv.put(prefix + "dbDriverName", tmp);

		// JDBC URL
		tmp=conf.get("dbSource");
		if(tmp==null) {
			throw new SQLException(
				"dbSource not given, invalid configuration.");
		}
		rv.put(prefix + "dbSource", tmp);

		// DB username
		tmp=conf.get("dbUser");
		if(tmp==null) {
			throw new SQLException(
				"dbUser not given, invalid configuration.");
		}
		rv.put(prefix + "dbUser", tmp);

		// DB username
		tmp=conf.get("dbPass");
		if(tmp==null) {
			throw new SQLException(
				"dbPass not given, invalid configuration.");
		}
		rv.put(prefix + "dbPass", tmp);

		// Add the db options
		for(Enumeration e=conf.propertyNames(); e.hasMoreElements(); ) {
			String pname=(String)e.nextElement();
			if(pname.startsWith("dboption.")) {
				String ovalue=conf.get(pname);
				rv.put(prefix + pname, ovalue);
			} // found a dboption
		} // All properties

		return(rv);
	}

}
