/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoHelper.java,v 1.1 1999/10/20 03:42:57 dustin Exp $
 */

package net.spy.photo;

import java.sql.*;
import java.util.*;
import java.lang.*;

import net.spy.*;

// The class
public class PhotoHelper
{ 
	SpyLog logger;
	PhotoConfig conf;

	public PhotoHelper() throws Exception {
		super();
		initlog();
		conf = new PhotoConfig();
	}

	protected void initlog() {
		logger = new SpyLog(new PhotoLogFlusher());
	}

	protected void log(String message) {
		System.err.println("PhotoHelper: " + message);
	}

	// Grab a connection from the pool.
	protected Connection getDBConn() throws Exception {
		SpyDB pdb=new SpyDB(new PhotoConfig(), false);
		return(pdb.getConn());
	}

	// Gotta free the connection
	protected void freeDBConn(Connection conn) {
		SpyDB pdb=new SpyDB(new PhotoConfig(), false);
		pdb.freeDBConn(conn);
	}
}
