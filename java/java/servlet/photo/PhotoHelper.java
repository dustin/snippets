/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoHelper.java,v 1.6 1999/10/10 19:54:57 dustin Exp $
 */

import java.sql.*;
import java.util.*;
import java.lang.*;

import com.javaexchange.dbConnectionBroker.*;


// The class
public class PhotoHelper
{
	DbConnectionBroker dbs;
	PhotoLogger logger;
	PhotoConfig conf;

	protected void getDBS() throws Exception {
		try {
			PhotoConfig conf = new PhotoConfig();
			Class.forName(conf.dbDriverName);
			dbs = new DbConnectionBroker(conf.dbDriverName,
				conf.dbSource, conf.dbUser, conf.dbPass,
				2, 5, "/tmp/pool.log", 0.01);
		} catch(Exception e) {
			// System.err.println("dbs broke:  " + e.getMessage());
			throw new Exception ("dbs broke: " + e.getMessage());
		}
	}

	protected void initlog() {
		logger = new PhotoLogger();
	}

	public PhotoHelper() throws Exception {
		super();
		getDBS();
		initlog();
		conf = new PhotoConfig();
	}

	public PhotoHelper(DbConnectionBroker db) {
		super();
		dbs=db;
		initlog();
		conf = new PhotoConfig();
	}

	protected void log(String message) {
		System.err.println("PhotoHelper: " + message);
	}

	// Grab a connection from the pool.
	protected Connection getDBConn() throws SQLException {
		Connection photo;

		// The path to the database...
		photo = dbs.getConnection();
		if(photo == null) {
			log("DB:  Could not get database connection");
		} else {
			log("DB:  Got connection:  " + dbs.idOfConnection(photo));
		}
		return(photo);
	}

	// Gotta free the connection
	protected void freeDBConn(Connection conn) {
		log("DB:  Freeing " + dbs.idOfConnection(conn));
		dbs.freeConnection(conn);
	}
}
