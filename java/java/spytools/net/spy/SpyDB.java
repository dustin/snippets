/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyDB.java,v 1.37 2002/07/10 04:24:38 dustin Exp $
 */

package net.spy;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;

import javax.naming.Context;
import javax.naming.InitialContext;

import javax.sql.DataSource;

import net.spy.pool.JDBCPoolFiller;
import net.spy.pool.ObjectPool;
import net.spy.pool.PooledObject;
import net.spy.pool.PoolException;
import net.spy.pool.PoolFiller;

/**
 * SpyDB is an abstraction of both net.spy.pool and java.sql.
 */

public class SpyDB extends Object {

	// Object pool we store our objects in.
	private static ObjectPool pool=null;
	// This isn't final because I want it to be referenced as a variable
	private static String POOL_MUTEX="POOL_MUTEX";

	// Place where we keep up with connections so we can get rid of them
	// manually if needed.
	private static Hashtable connections=null;
	// This isn't final because I want it to be referenced as a variable
	private static String CONNECTIONS_MUTEX="CONNECTIONS_MUTEX";

	// Pooled Object container we got out of the pool.
	private PooledObject object=null;

	// The actual database connection from the PooledObject.
	private Connection conn=null;

	// If there's an exception at initialization time, store it here.
	private Exception initializationException=null;

	// Our configuration.
	private SpyConfig conf = null;

	// Pool name.
	private String name=null;

	// Whether we want the object to free stuff or not.
	private boolean auto_free=true;

	// Whether we want to use a pool.
	private boolean use_pool=true;
	private boolean use_jndi=true;

	// Is this thing closed?
	private boolean isClosed=false;

	/**
	 * Create a SpyDB object based on the description found in the passed
	 * in SpyConfig object.  The following lists the minimal requirement
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
	 *  <li>dbPoolType - what type of pool to use (SpyPool, jndi, none)
	 *      - default SpyPool</li>
	 *  <li>dbPoolName - default db</li>
	 *  <li>dbMinConns - minimum number of connections - default 1</li>
	 *  <li>dbStartConns - minimum number of connections - default 1</li>
	 *  <li>dbYellowLine - the pool's ``yellow line'' percentage
	 *      - default 75</li>
	 *  <li>dbMaxConns - maximum number of connections - default 5</li>
	 *  <li>dbMaxLifeTime - maximum connection lifetime in milliseconds -
	 *      default 6 hours</li>
	 * </ul>
	 *
	 * @param conf SpyConfig object describing how to connect.
	 */
	public SpyDB(SpyConfig conf) {
		this(conf, true);
	}

	/**
	 * Get a SpyDB object wrapping the given connection.
	 *
	 * @param conn the connection to wrap.
	 */
	public SpyDB(Connection conn) {
		super();
		this.conn=conn;
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
			initializationException=e;
			log("Error initializing SpyDB:  " + e);
			e.printStackTrace();
		}
		// System.out.println("Debug:  " + pool + "\n" + connections);
	}

	// Warning, this contains a bunch of nasty backward-compatibility
	// stuff.
	private synchronized void initStuff() throws PoolException, SQLException {

		// If we haven't established our connections hash yet, do so
		// Synchronize on the class to do this
		synchronized(CONNECTIONS_MUTEX) {
			if(connections==null) {
				connections=new Hashtable();
			}
		}

		// Figure out whether we want to pool.
		String tmp=conf.get("dbPoolType");
		if(tmp!=null) {

			if(tmp.equalsIgnoreCase("jndi")) {
				use_pool=false;
				use_jndi=true;
			} else if(tmp.equalsIgnoreCase("none")) {
				use_pool=false;
				use_jndi=false;
			} else {
				use_pool=true;
				use_jndi=false;
			}
		}

		// If we'll be using a pool, get it initialized.
		if(use_pool) {
			// Poolname.
			name=conf.get("dbPoolName");
			if(name==null) {
				name=conf.get("dbcbLogFilePath", "db");
			}

			// Synchronize this whole thing so we don't accidentally try to
			// create it more than once.
			synchronized(POOL_MUTEX) {
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
		}

	}

	// Get a normalized config from the one we already have
	private SpyConfig getNormalizedConfig() throws SQLException {
		// We'll need a config to translate into
		SpyConfig tmpconf=new SpyConfig();

		String prefix="";
		if(name!=null) {
			prefix=name + ".";
		}

		// Minimum connections in the pool.
		int min_conns=conf.getInt("dbMinConns", -1);
		if(min_conns==-1) {
			min_conns=conf.getInt("dbcbMinConns", 1);
		}
		tmpconf.put(prefix + "min", "" + min_conns);

		// Start this many connections in the pool.
		int start_conns=conf.getInt("dbStartConns", min_conns);
		tmpconf.put(prefix + "start", "" + start_conns);

		// Yellow line percentage
		int yellow_line=conf.getInt("dbYellowLine", -1);
		if(yellow_line>0) {
			tmpconf.put(prefix + "yellow_line", "" + yellow_line);
		}

		// maximum connections in the pool.
		int max_conns=conf.getInt("dbMaxConns", -1);
		if(max_conns==-1) {
			max_conns=conf.getInt("dbcbMaxConns", 5);
		}
		tmpconf.put(prefix + "max", "" + max_conns);

		// Maximum amount of time any given entry may live.
		String tmp=conf.get("dbMaxLifeTime");
		if(tmp==null) {
			tmp=conf.get("dbcbMaxLifeTime");
			if(tmp!=null) {
				double tmpd=Double.valueOf(tmp).doubleValue();
				tmpd*=86400;
				long recycle_time=(long)tmpd;
				tmpconf.put(prefix + "max_age", "" + recycle_time*1000);
			}
		} else {
			// The dbMaxLifeTime was valid, put it place.
			tmpconf.put(prefix + "max_age", tmp);
		}

		// Driver name.
		tmp=conf.get("dbDriverName");
		if(tmp==null) {
			throw new SQLException(
				"dbDriverName not given, invalid configuration.");
		}
		tmpconf.put(prefix + "dbDriverName", tmp);
		// JDBC URL
		tmp=conf.get("dbSource");
		if(tmp==null) {
			throw new SQLException(
				"dbSource not given, invalid configuration.");
		}
		tmpconf.put(prefix + "dbSource", tmp);
		// username
		tmp=conf.get("dbUser");
		if(tmp==null) {
			throw new SQLException(
				"dbUser not given, invalid configuration.");
		}
		tmpconf.put(prefix + "dbUser", tmp);
		// password
		tmp=conf.get("dbPass");
		if(tmp==null) {
			throw new SQLException(
				"dbPass not given, invalid configuration.");
		}
		tmpconf.put(prefix + "dbPass", tmp);

		return(tmpconf);
	}

	// Create a pool
	private void createPool() throws PoolException, SQLException {
		// Get a conf
		SpyConfig conf=getNormalizedConfig();

		// If we don't yet have a pool at all, create one.
		synchronized(POOL_MUTEX) {
			if(pool==null) {
				pool=new ObjectPool(conf);
			}

			// Set the database options:
			setDBOptions(conf);

			// Grab the poolfiller with our temporary config.
			JDBCPoolFiller pf=new JDBCPoolFiller(name, conf);
			// OK, add the pool.
			pool.createPool(name, pf);
		}
	}

	// set options prefixed with the name
	private void setDBOptions(SpyConfig tmpconf) {
		setDBOptions(name + ".", tmpconf);
	}

	// Set DB options
	private void setDBOptions(String prefix, SpyConfig tmpconf) {
		for(Enumeration e=conf.propertyNames(); e.hasMoreElements(); ) {
			String pname=(String)e.nextElement();
			if(pname.startsWith("dboption.")) {
				String ovalue=conf.get(pname);
				if(prefix==null) {
					tmpconf.put(pname, ovalue);
				} else {
					tmpconf.put(prefix + pname, ovalue);
				}
			} // found a dboption
		} // For all properties
	}

	/**
	 * Execute a query and return a resultset, will establish a database
	 * connection if necessary.
	 *
	 * @param query SQL query to execute.
	 *
	 * @exception SQLException an exception is thrown if the connection fails,
	 * or the SQL query fails.
	 */
	public ResultSet executeQuery(String query) throws SQLException {
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
	 * @exception SQLException an exception is thrown if the connection fails,
	 * or the SQL query fails.
	 */
	public int executeUpdate(String query) throws SQLException {
		int rv=0;
		Connection conn=getConn();
		Statement st = conn.createStatement();
		rv=st.executeUpdate(query);
		return(rv);
	}

	/**
	 * Prepare a statement.
	 *
	 * @param query SQL query to prepare.
	 *
	 * @exception SQLException thrown if something bad happens.
	 */
	public PreparedStatement prepareStatement(String query)
		throws SQLException {

		Connection conn=getConn();
		PreparedStatement pst = conn.prepareStatement(query);
		return(pst);
	}

	/**
	 * Get a connection out of the pool.  A given SpyDB object can only
	 * maintain a single database connection, so if multiple connections
	 * from the pool are needed, multiple SpyDB objects will be required.
	 *
	 * @exception SQLException An exception may be thrown if a database
	 * connection cannot be obtained.
	 */
	public Connection getConn() throws SQLException {
		if(conn==null) {
			// log("New connection");
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
		// Check in the object if we have one
		if(object!=null) {
			object.checkIn();
			object=null;
		}
		// We may want to remove this from our connection hash.
		if(conn!=null) {
			connections.remove(conn);
			// If we're not using a pool, we need to close the connection.
			if(!use_pool) {
				try {
					conn.close();
				} catch(Exception e) {
					e.printStackTrace();
				}
			}
			conn=null;
		}
	}

	/**
	 * Free a database connection that was not in auto_free mode.
	 */
	public void freeDBConn(Connection conn) {
		if(use_pool) {
			PooledObject po=(PooledObject)connections.get(conn);
			if(po!=null) {
				po.checkIn();
			}
		}

		// Remove it from our hash
		connections.remove(conn);

		// If we're not using a pool, we need to close the connection.
		if(!use_pool) {
			try {
				conn.close();
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
		this.conn=null;
	}

	/**
	 *  Free an established database connection - alias to freeDBConn()
	 */
	public void close() {
		freeDBConn();
		isClosed=true;
	}

	/**
	 * Initialize SpyDB.  Currently, this does nothing.
	 */
	public void init() {
	}

	// This is a debug routine
	private void log(String msg) {
		// System.err.println("DB:  " + msg);
	}

	private void getDBConnFromSpyPool() throws SQLException {
		try {
			if(initializationException==null) {
				synchronized(POOL_MUTEX) {
					object=pool.getObject(name);
				}
				conn=(Connection)object.getObject();
				if(!auto_free) {
					connections.put(conn, object);
				}
			} else {
				throw initializationException;
			}
		} catch(Exception e) {
			e.printStackTrace();
			throw new SQLException(
				"Unable to get database connection:  " + e);
		}
	}

	private void getDBConnFromJDBC() throws SQLException {
		SpyConfig conf=getNormalizedConfig();
		setDBOptions("", conf);

		// Make sure the username and password are set
		conf.put("user", conf.get("dbUser"));
		conf.put("password", conf.get("dbPass"));

		// Figure out what this guy's trying to get us to do.
		String driver=conf.get("dbDriverName");
		String source=conf.get("dbSource");

		// Try to load the class
		try {
			Class.forName(driver);
		} catch(Exception e) {
			throw new SQLException("Can't load class for db connection: "
				+ e);
		}

		// Try to get the actual connection
		conn=DriverManager.getConnection(source, conf);
	}

	private void getDBConnFromJNDI() throws SQLException {
		// Make sure the username and password are set
		// Figure out what this guy's trying to get us to do.
		String source=conf.get("dbSource");

		try {
			Context initial = new InitialContext();
			DataSource dsrc = (DataSource)initial.lookup(source);
			conn = dsrc.getConnection();
		} catch(Exception e) {
			e.printStackTrace();
			throw new SQLException("Error getting connection from JNDI:  " + e);
		}
	}

	private void getDBConn() throws SQLException {
		// Different behavior whether we're using a pool or not
		if(use_pool) {
			getDBConnFromSpyPool();
		} else if(use_jndi) {
			getDBConnFromJNDI();
		} else {
			getDBConnFromJDBC();
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

	/**
	 * Has close() been called?
	 */
	public boolean isClosed() {
		return(isClosed);
	}

}
