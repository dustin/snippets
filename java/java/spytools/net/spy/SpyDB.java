/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyDB.java,v 1.21 2000/11/02 22:19:37 dustin Exp $
 */

package net.spy;

import java.sql.*;
import java.util.*;

import net.spy.pool.*;

/**
 * SpyDB is an abstraction of both net.spy.pool and java.sql.
 */

public class SpyDB extends Object {

	// Object pool we store our objects in.
	protected static ObjectPool pool=null;
	// This isn't final because I want it to be referenced as a variable
	protected static String POOL_MUTEX="POOL_MUTEX";

	// Place where we keep up with connections so we can get rid of them
	// manually if needed.
	protected static Hashtable connections=null;
	// This isn't final because I want it to be referenced as a variable
	protected static String CONNECTIONS_MUTEX="CONNECTIONS_MUTEX";

	// Pooled Object container we got out of the pool.
	protected PooledObject object=null;

	// The actual database connection from the PooledObject.
	protected Connection conn=null;

	// Our configuration.
	protected SpyConfig conf = null;

	// Pool name.
	protected String name=null;

	// Number of connections to start with.
	protected int min_conns = 1;
	// Maximum number of connections to open.
	protected int max_conns = 5;
	// How long (in milliseconds) to keep a connection open.
	protected long recycle_time = 6 * 3600 * 60 * 1000;

	// Whether we want the object to free stuff or not.
	protected boolean auto_free=true;

	/**
	 * Create a SpyDB object based on the description found in the passed
	 * in SpyConfig object.  The following lists the minimal requiremenet
	 * for configuration:
	 * <p>
	 * <ul>
	 *  <li>dbDriverName - Driver to load (i.e. org.postgresql.Driver)</li>
	 *  <li>dbSource - JDBC URL we'll be connecting to.</li>
	 *  <li>dbUser - Database username</li>
	 *  <li>dbPass - Database password</li>
	 * </ul>
	 *
	 * The following config entries are optional, but supported:
	 * <p>
	 * <ul>
	 *  <li>dbPoolName - default db</li>
	 *  <li>dbMinConns - minimum number of connections - default 1</li>
	 *  <li>dbMaxConns - maximum number of connections - default 5</li>
	 *  <li>dbMaxLifeTime - maximum connection lifetime in milliseconds -
	 *      default 6 hours</li>
	 * </ul>
	 *
	 * @param conf SpyConfig object describing how to connect.
	 */
	public SpyDB(SpyConfig conf) {
		this.conf=conf;

		try {
			initStuff();
		} catch(Exception e) {
			log("Error initializing SpyDB:  " + e);
			e.printStackTrace();
		}
		// System.out.println("Debug:  " + pool + "\n" + connections);
	}

	/**
	 * Same as the above constructor, but has a boolean allowing you to
	 * disable auto_free.
	 *
	 * @param conf SpyConfig object describing how to connect.
	 *
	 * @param auto_free is pretty much ignored now, as things will always
	 * automatically free if not freed explicitly.
	 */
	public SpyDB(SpyConfig conf, boolean auto_free) {
		this.conf=conf;
		this.auto_free=auto_free;
		try {
			initStuff();
		} catch(Exception e) {
			log("Error initializing SpyDB:  " + e);
			e.printStackTrace();
		}
		// System.out.println("Debug:  " + pool + "\n" + connections);
	}

	// Warning, this contains a bunch of nasty backward-compatibility
	// stuff.
	protected synchronized void initStuff() throws PoolException {

		// If we haven't established our connections hash yet, do so
		// Synchronize on the class to do this
		synchronized(CONNECTIONS_MUTEX) {
			if(connections==null) {
				connections=new Hashtable();
			}
		}

		// Poolname.
		name=conf.get("dbPoolName");
		if(name==null) {
			name=conf.get("dbcbLogFilePath", "db");
		}

		// if we don't have a pool, create one.
		if(pool==null) {
			createPool();
		} else {
			// If we have a pool, but not the one we're looking
			// for...create it.
			if(!pool.hasPool(name)) {
				createPool();
			}
		}

	}

	protected void createPool() throws PoolException {
		// We'll need a config to translate into
		SpyConfig tmpconf=new SpyConfig();

		// If we don't yet have a pool at all, create one.
		synchronized(POOL_MUTEX) {
			if(pool==null) {
				pool=new ObjectPool(conf);
			}
		}

		// Minimum connections in the pool.
		min_conns=conf.getInt("dbMinConns", -1);
		if(min_conns==-1) {
			min_conns=conf.getInt("dbcbMinConns", 1);
		}
		tmpconf.put(name + ".min", "" + min_conns);

		// maximum connections in the pool.
		max_conns=conf.getInt("dbMaxConns", -1);
		if(max_conns==-1) {
			max_conns=conf.getInt("dbcbMaxConns", 5);
		}
		tmpconf.put(name + ".max", "" + max_conns);

		// Maximum amount of time any given entry may live.
		String tmp=conf.get("dbMaxLifeTime");
		if(tmp==null) {
			tmp=conf.get("dbcbMaxLifeTime");
			if(tmp!=null) {
				double tmpd=Double.valueOf(tmp).doubleValue();
				tmpd*=86400;
				recycle_time=(long)tmpd;
				tmpconf.put(name + ".max_age", "" + recycle_time*1000);
			}
		} else {
			// The dbMaxLifeTime was valid, put it place.
			tmpconf.put(name + ".max_age", tmp);
		}

		// Driver name.
		tmpconf.put(name + ".dbDriverName", conf.get("dbDriverName"));
		// JDBC URL
		tmpconf.put(name + ".dbSource", conf.get("dbSource"));
		// username
		tmpconf.put(name + ".dbUser", conf.get("dbUser"));
		// password
		tmpconf.put(name + ".dbPass", conf.get("dbPass"));

		// Grab the poolfiller with our temporary config.
		JDBCPoolFiller pf=new JDBCPoolFiller(name, tmpconf);
		// OK, add the pool.
		pool.createPool(name, pf);
	}

	/**
	 * Execute a query and return a resultset, will establish a database
	 * connection if necessary.
	 *
	 * @param query SQL query to execute.
	 *
	 * @exception Exception an exception is thrown if the connection fails, or
	 * the SQL query fails.
	 */
	public ResultSet executeQuery(String query) throws Exception {
		Connection conn=getConn();
		Statement st = conn.createStatement();
		ResultSet rs = st.executeQuery(query);
		return(rs);
	}

	/**
	 * Execute a query that doesn't return a ResultSet, such as an update,
	 * delete, or insert.
	 *
	 * @param query SQL query to execute.
	 *
	 * @exception Exception an exception is thrown if the connection fails, or
	 * the SQL query fails.
	 */
	public void executeUpdate(String query) throws Exception {
		Connection conn=getConn();
		Statement st = conn.createStatement();
		st.executeUpdate(query);
	}

	/**
	 * Prepare a statement.
	 *
	 * @param query SQL query to prepare.
	 *
	 * @exception Exception thrown if something bad happens.
	 */
	public PreparedStatement prepareStatement(String query) throws Exception {
		Connection conn=getConn();
		PreparedStatement pst = conn.prepareStatement(query);
		return(pst);
	}

	/**
	 * Get a connection out of the pool.  A given SpyDB object can only
	 * maintain a single database connection, so if multiple connections
	 * from the pool are needed, multiple SpyDB objects will be required.
	 *
	 * @exception Exception An exception may be thrown if a database
	 * connection cannot be obtained.
	 */
	public Connection getConn() throws Exception {
		if(conn==null) {
			log("New connection");
			getDBConn();
		}
		return(conn);
	}

	/**
	 * Free an established database connection.  The connection is whatever
	 * connection has already been instablished by this instance of the
	 * object.  If a connection has not been established, this does
	 * nothing.
	 * <p>
	 * Note:  This should rarely be called directly.
	 */
	public void freeDBConn() {
		log("Freeing");
		if(object!=null) {
			object.checkIn();
			object=null;
			// We may want to remove this from our connection hash.
			if(conn!=null) {
				connections.remove(conn);
			}
		}
	}

	/**
	 * Free a database connection that was not in auto_free mode.
	 */
	public void freeDBConn(Connection conn) {
		PooledObject po=(PooledObject)connections.get(conn);
		if(po!=null) {
			po.checkIn();
			connections.remove(conn);
		}
		this.conn=null;
	}

	/**
	 *  Free an established database connection - alias to freeDBConn()
	 */
	public void close() {
		freeDBConn();
	}

	/**
	 * Initialize SpyDB.  Currently, this does nothing.
	 */
	public void init() {
	}

	protected void log(String msg) {
		System.err.println("DB:  " + msg);
	}

	protected void getDBConn() throws SQLException {
		try {
			object=pool.getObject(name);
			conn=(Connection)object.getObject();
			if(!auto_free) {
				connections.put(conn, object);
			}
		} catch(Exception e) {
			throw new SQLException("Unable to get database connection:  " + e);
		}
	}

	/**
	 * Make a string safe for usage in a SQL query, quoting apostrophies,
	 * etc...
	 *
	 * @param in the string that needs to be quoted
	 *
	 * @return a new, quoted string
	 */
	public static String dbquote_str(String in) {
		// Quick...handle null
		if(in == null) {
			return(null);
		}

		String scopy = new String(in);
		if(scopy.indexOf('\'') >= 0) {
			String sout = new String("");
			StringTokenizer st = new StringTokenizer(scopy, "\'");
			while(st.hasMoreTokens()) {
				String part = st.nextToken();

				if(st.hasMoreTokens()) {
					sout += part + "\'\'";
				} else {
					sout += part;
				}
			}
			scopy=sout;
		}
		return(scopy);
	}
}
