/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoHelper.java,v 1.3 1999/09/29 07:15:04 dustin Exp $
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

	protected void getDBS() throws Exception {
		try {
			Class.forName("postgresql.Driver");
			String source="jdbc:postgresql://dhcp-104/photo";
			dbs = new DbConnectionBroker("postgresql.Driver",
				source, "dustin", "", 2, 5, "/tmp/pool.log", 0.01);
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
	}

	public PhotoHelper(DbConnectionBroker db) {
		super();
		dbs=db;
		initlog();
	}

	protected void log(String message) {
		System.err.println("PhotoHelper: " + message);
	}

	// Grab a connection from the pool.
	protected Connection getDBConn() throws SQLException {
		Connection photo;

		// The path to the database...
		photo = dbs.getConnection();
		return(photo);
	}

	// Gotta free the connection
	protected void freeDBConn(Connection conn) {
		dbs.freeConnection(conn);
	}
}
