/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoLogFlusher.java,v 1.3 2000/01/25 06:58:56 dustin Exp $
 */

package net.spy.photo;

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;

import net.spy.*;

public class PhotoLogFlusher extends SpyLogFlusher {

	public PhotoLogFlusher() {
		super();
	}

	public synchronized void doFlush() {
		Vector v = log_object.flush();
		Statement st = null;
		Connection db=null;
		SpyDB photodb=null;
		// Only do all this crap if there's something to log.
		if(v.size() > 0) {
			try {
				photodb = new SpyDB(new PhotoConfig());
				db=photodb.getConn();
				st=db.createStatement();
				for(int i = 0; i<v.size(); i++) {
					SpyLogEntry l = null;
					try {
						l = (SpyLogEntry)v.elementAt(i);
							st.executeUpdate(l.toString());
					} catch(SQLException e) {
						System.err.println("Error writing log:  "
							+ l + e.getMessage());
					}
				}
			} catch(Exception e) {
				System.err.println("BAD LOG ERRROR!  " + e.getMessage());
			} finally {
				photodb.freeDBConn();
			}
		}
	}
}
