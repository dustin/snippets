/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoHelper.java,v 1.7 1999/10/12 22:54:04 dustin Exp $
 */

import java.sql.*;
import java.util.*;
import java.lang.*;

// The class
public class PhotoHelper
{
	PhotoLogger logger;
	PhotoConfig conf;

	public PhotoHelper() throws Exception {
		super();
		initlog();
		conf = new PhotoConfig();
	}

	protected void initlog() {
		logger = new PhotoLogger();
	}

	protected void log(String message) {
		System.err.println("PhotoHelper: " + message);
	}

	// Grab a connection from the pool.
	protected Connection getDBConn() throws Exception {
		PhotoDB pdb=new PhotoDB();
		return(pdb.getConn());
	}

	// Gotta free the connection
	protected void freeDBConn(Connection conn) {
		PhotoDB pdb=new PhotoDB();
		pdb.freeDBConn(conn);
	}
}
