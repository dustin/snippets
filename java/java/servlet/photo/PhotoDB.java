/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoDB.java,v 1.1 1999/10/12 22:58:32 dustin Exp $
 */

import java.sql.*;
import java.util.*;

import com.javaexchange.dbConnectionBroker.*;

public class PhotoDB extends Object {

	private static DbConnectionBroker dbs=null;
	Connection conn=null;

	public PhotoDB() {
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
		if(conn==null) {
			log("conn is null, need to reinit");
			initDBS();
			conn=dbs.getConnection();
			if(conn==null) {
				log("conn is *still* null, need to reinit");
				initDBS();
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
			PhotoConfig pconfig = new PhotoConfig();
			Class.forName(pconfig.dbDriverName);
			dbs = new DbConnectionBroker(pconfig.dbDriverName,
				pconfig.dbSource, pconfig.dbUser, pconfig.dbPass,
				3, 6, "/tmp/pool.log", 0.01);
		} catch(Exception e) {
			// Do nothing
			// throw new Exception("dbs broke:  " + e);
		}
	}
}
