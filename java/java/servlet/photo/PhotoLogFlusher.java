/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoLogFlusher.java,v 1.6 1999/10/20 02:14:53 dustin Exp $
 */

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;

import net.spy.*;

public class PhotoLogFlusher extends SpyLogFlusher {

	public PhotoLogFlusher(ThreadGroup t) {
		super(t);
		this.setDaemon(true);
	}

	public PhotoLogFlusher() {
		super(new ThreadGroup(SpyLog.getSystemGroup(), "logging"));
		this.setDaemon(true);
	}

	public synchronized void doFlush() {
		Vector v = log_object.flush();
		Statement st = null;
		Connection db=null;
		PhotoDB photodb=null;
		// Only do all this crap if there's something to log.
		if(v.size() > 0) {
			try {
				photodb = new PhotoDB();
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
				photodb.freeDBConn(db);
			}
		}
	}
}
