/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyDB.java,v 1.4 1999/12/15 03:58:14 dustin Exp $
 */

package net.spy;

import java.sql.*;
import java.util.*;

import com.javaexchange.dbConnectionBroker.*;

public class SpyDB extends Object {

	protected static DbConnectionBroker dbs=null;
	Connection conn=null;
	SpyConfig conf = null;
	boolean auto_free = true;

	public SpyDB(SpyConfig conf) {
		this.conf=conf;
		if(dbs==null) {
			initDBS();
		}
	}

	public SpyDB(SpyConfig conf, boolean auto_free) {
		this.conf=conf;
		this.auto_free=auto_free;
		if(dbs==null) {
			initDBS();
		}
	}

	public Connection getConn() throws Exception {
		log("New connection");
		if(conn==null) {
			getDBConn();
		}
		return(conn);
	}

	public void freeDBConn() {
		log("Freeing");
		dbs.freeConnection(conn);
	}

	public void freeDBConn(Connection c) {
		log("Freeing");
		dbs.freeConnection(c);
	}

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
		String log_file = conf.get("dbcbLogFilePath");
		if(log_file == null) {
			log_file = "/tmp/pool.log";
		}
		// Load the db driver
		try {
			Class.forName(conf.get("dbDriverName"));
			dbs = new DbConnectionBroker(conf.get("dbDriverName"),
				conf.get("dbSource"), conf.get("dbUser"), conf.get("dbPass"),
				3, 20, log_file, 0.01);
		} catch(Exception e) {
			// Do nothing
			// throw new Exception("dbs broke:  " + e);
		}
	}

	public static String dbquote_str(String thing) {
		// Quick...handle null
		if(thing == null) {
			return(null);
		}

		String scopy = new String(thing);
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

	// Free!
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
