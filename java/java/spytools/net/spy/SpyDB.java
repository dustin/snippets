/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyDB.java,v 1.6 2000/01/24 06:40:30 dustin Exp $
 */

package net.spy;

import java.sql.*;
import java.util.*;

import com.javaexchange.dbConnectionBroker.*;

/**
 * SpyDB is an abstraction of both JavaExchange's DB Connection Broker, and
 * java.sql.
 * <p>
 * For more information on DB Connection Broker, check out
 * <a href="http://www.javaexchange.com/">Java Exchange</a>.
 */

public class SpyDB extends Object {

	protected static Hashtable dbss;
	protected Connection conn=null;
	protected SpyConfig conf = null;
	protected String log_file=null;
	protected DbConnectionBroker dbs=null;
	protected boolean auto_free = true;

	/**
	 * Create a SpyDB object based on the description found in the passed
	 * in SpyConfig object.  The following lists the minimal requiremenet
	 * for configuration:
	 * <p>
	 * <ul>
	 *  <li>dbDriverName - Driver to load (i.e. postgresql.Driver)</li>
	 *  <li>dbSource - JDBC URL we'll be connecting to.</li>
	 *  <li>dbUser - Database username</li>
	 *  <li>dbPass - Database password</li>
	 * </ul>
	 *
	 * @param conf SpyConfig object describing how to connect.
	 */
	public SpyDB(SpyConfig conf) {
		this.conf=conf;

		log_file=conf.get("dbcbLogFilePath");
		if(log_file==null) {
			log_file="/tmp/pool.log";
		}
		if(dbss==null) {
			dbss=new Hashtable();
		}
		dbs=(DbConnectionBroker)dbss.get(log_file);
		if(dbs==null) {
			initDBS();
		}

	}

	/**
	 * Same as the above constructor, but has a boolean allowing you to
	 * disable auto_free.
	 *
	 * @param conf SpyConfig object describing how to connect.
	 *
	 * @param auto_free If false, all database connections must be manually
	 * freed.
	 */
	public SpyDB(SpyConfig conf, boolean auto_free) {
		this.conf=conf;
		this.auto_free=auto_free;

		log_file=conf.get("dbcbLogFilePath");
		if(log_file==null) {
			log_file="/tmp/pool.log";
		}
		if(dbss==null) {
			dbss=new Hashtable();
		}
		dbs=(DbConnectionBroker)dbss.get(log_file);
		if(dbs==null) {
			initDBS();
		}

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
	 * Get a connection out of the pool.  A given SpyDB object can only
	 * maintain a single database connection, so if multiple connections
	 * from the pool are needed, multiple SpyDB objects will be required.
	 *
	 * @exception Exception An exception may be thrown if a database
	 * connection cannot be obtained.
	 */
	public Connection getConn() throws Exception {
		log("New connection");
		if(conn==null) {
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
		if(conn!=null) {
			dbs.freeConnection(conn);
			conn=null;
		}
	}

	/**
	 * Free an established database connection.  This method allows you to
	 * free a specific database connection by passing it in.
	 * <p>
	 * Note:  This should be called more rarely than the last method.
	 */
	public void freeDBConn(Connection c) {
		log("Freeing");
		dbs.freeConnection(c);
	}

	/**
	 * Initialize SpyDB.  This will completely reinitialize SpyDB and all
	 * pools.
	 */
	public void init() {
		try {
			dbs=null;
		} catch(Exception e) {
			// Nothing
		}
	}

	protected void log(String msg) {
		System.err.println("DB:  " + msg);
	}

	protected void getDBConn() throws SQLException {
		log("Getting a connection");
		if(dbs == null) {
			log("dbs is null, need to reinit");
			initDBS();
		}
		conn=dbs.getConnection();
		// Make sure we got one, *and* it's open.
		if(conn==null || conn.isClosed()) {
			log("conn is null, trying to run finalization");
			System.runFinalization();
			conn=dbs.getConnection();
			if(conn==null || conn.isClosed()) {
				log("conn is null again, reinitializing");
				initDBS();
				conn=dbs.getConnection();
				if(conn==null || conn.isClosed()) {
					log("conn is *still* null, need to reinit");
					initDBS();
				}
			}
		}
	}

	protected void initDBS() {
		log("Initializing");
		if(dbs!=null) {
			dbs.destroy();
		}
		// Nullify it.
		dbs=null;
		// Get rid of garbage.
		System.runFinalization();
		System.gc();

		// Load the db driver
		try {
			Class.forName(conf.get("dbDriverName"));
			dbs = new DbConnectionBroker(conf.get("dbDriverName"),
				conf.get("dbSource"), conf.get("dbUser"), conf.get("dbPass"),
				3, 20, log_file, 0.01);
			dbss.put(log_file, dbs);
			log("Got a new DBCB object logging to " + log_file);
		} catch(Exception e) {
			// Do nothing
			// throw new Exception("dbs broke:  " + e);
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
	 * Object finalization.  Ignore this, please.
	 *
	 * @exception Throwable yeah, it could throw an exception.
	 */
	public void finalize() throws Throwable {
		if(auto_free && conn!=null) {
			try {
				freeDBConn();
			} catch(Exception e) {
				// Nothin'
			}
		}
		super.finalize();
	}
}
