/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyDB.java,v 1.40 2002/08/03 06:07:03 dustin Exp $
 */

package net.spy;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import java.util.StringTokenizer;

import net.spy.db.ConnectionSource;

/**
 * SpyDB is an abstraction of both net.spy.pool and java.sql.
 */

public class SpyDB extends Object {

	// The actual database connection from the PooledObject.
	private Connection conn=null;

	// Our configuration.
	private SpyConfig conf = null;

	// Is this thing closed?
	private boolean isClosed=false;

	// The connection source.
	private ConnectionSource source=null;

	// Exceptions that occur during initialization.
	private Exception initializationException=null;

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
	 *  <li>dbConnectionSource - the ConnectionSource class responsible for
	 *      getting DB connections for this SpyDB
	 *      - default net.spy.db.ObjectPoolConnectionSource</li>
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
		super();
		this.conf=conf;

		initialize();
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
	 */
	public void freeDBConn() {
		// Don't touch it unless it came from a ConnectionSource
		if(source!=null) {
			// If there's a source, this came from something that will want
			// to hear that we're done with it.
			if(conn!=null) {
				try {
					source.returnConnection(conn);
				} catch(SQLException e) {
					e.printStackTrace();
				}
			}
		}
		isClosed=true;
	}

	/**
	 * Free an established database connection - alias to freeDBConn()
	 */
	public void close() {
		freeDBConn();
	}

	private void initialize() {
		String connectionClassName=conf.get("dbConnectionSource",
			"net.spy.db.ObjectPoolConnectionSource");
		// Backwards compatibilify
		if(connectionClassName == null) {
			String tmp=conf.get("dbPoolType");
			if(tmp!=null) {
				if(tmp.equals("jndi")) {
					connectionClassName="net.spy.db.JNDIConnectionSource";
				} else if(tmp.equals("none")) {
					connectionClassName="net.spy.db.JDBCConnectionSource";
				} else {
					// My pool
					connectionClassName="net.spy.db.ObjectPoolConnectionSource";
				}
			}
		}
		// OK, we now know *how* we're going to get connections, let's get
		// the source object.
		try {
			Class connectionSourceClass=Class.forName(connectionClassName);
			source=(ConnectionSource)connectionSourceClass.newInstance();
		} catch(Exception e) {
			e.fillInStackTrace();
			initializationException=e;
		}
		init();
	}

	/**
	 * Initialize SpyDB.  This allows any subclasses to perform further
	 * initialization.
	 */
	protected void init() {
	}

	// This is a debug routine
	private void log(String msg) {
		// System.err.println("DB:  " + msg);
	}

	// Actually dig up a DB connection
	private void getDBConn() throws SQLException {
		// Different behavior whether we're using a pool or not

		if(initializationException!=null) {
			initializationException.printStackTrace();
			throw new SQLException("Initialization exception:  "
				+ initializationException);
		}

		// Get the connection from the source.
		conn=source.getConnection(conf);
	}

	/**
	 * @deprecated use dbquoteStr instead
	 */
	public static String dbquote_str(String in) {
		return(dbquoteStr(in));
	}

	/**
	 * Make a string safe for usage in a SQL query, quoting apostrophies,
	 * etc...
	 *
	 * @param in the string that needs to be quoted
	 *
	 * @return a new, quoted string
	 */
	public static String dbquoteStr(String in) {
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
