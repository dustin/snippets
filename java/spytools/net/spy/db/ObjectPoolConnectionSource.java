// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPoolConnectionSource.java,v 1.5 2002/08/23 17:25:42 dustin Exp $

package net.spy.db;

import java.util.Enumeration;

import java.sql.Connection;
import java.sql.SQLException;

import net.spy.SpyConfig;

import net.spy.pool.ObjectPool;
import net.spy.pool.PooledObject;
import net.spy.pool.PoolException;
import net.spy.pool.JDBCPoolFiller;
import net.spy.pool.NoSuchPoolException;

/**
 * Connection source to retrieve connections from an ObjectPool.
 *
 * The configuration passed into getConnection() requires the following
 * parameters:
 *
 * <ul>
 *  <li>dbDriverName - Driver to load (i.e. org.postgresql.Driver)</li>
 *  <li>dbSource - dbUser</li>
 *  <li>dbUser - Database username</li>
 *  <li>dbPass - Database password</li>
 * </ul>
 *
 * The following parameters are optional:
 *
 * <ul>
 *  <li>dbPoolName - default: <i>db</i></li>
 *  <li>dbMinConns - minimum number of connections - default 1</li>
 *  <li>dbStartConns - minimum number of connections - default 1</li>
 *  <li>dbYellowLine - the pool's ``yellow line'' percentage
 *      - default 75</li>
 *  <li>dbMaxConns - maximum number of connections - default 5</li>
 *  <li>dbMaxLifeTime - maximum connection lifetime in milliseconds -
 *      default 6 hours</li>
 * </ul>
 */
public class ObjectPoolConnectionSource extends Object
	implements ConnectionSource {

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

		// If there's no pool at all, it's not initialized.
		if(pool==null) {
			initialize(conf);
		}

		Connection conn=null;
		try {
			// Snatch the pebble from my hand.
			conn=getConn(poolName);
		} catch(NoSuchPoolException pe) {
			// If the pool we're looking for doesn't exist, initialize.
			initialize(conf);
			try {
				conn=getConn(poolName);
			} catch(PoolException e) {
				e.printStackTrace();
				throw new SQLException("Could not get a DB connection:  " + pe);
			}
		} catch(PoolException pe) {
			pe.printStackTrace();
			throw new SQLException("Could not get a DB connection:  " + pe);
		}
		return(conn);
	}

	// Do the pool work.
	private Connection getConn(String poolName)
		throws SQLException, PoolException {

		Connection rv=null;
		object=pool.getObject(poolName);
		rv=(Connection)object.getObject();

		return(rv);
	}

	/**
	 * @see ConnectionSource
	 */
	public void returnConnection(Connection conn) throws SQLException {
		if(object==null) {
			throw new SQLException("Object is null, already returned?");
		}
		object.checkIn();
		object=null;
	}

	// Perform one-time initialization for a config.
	private void initialize(SpyConfig gConf)
		throws SQLException {

		synchronized(ObjectPoolConnectionSource.class) {
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
					throw new SQLException("Error initializing pool:  " + pe);
				}
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
