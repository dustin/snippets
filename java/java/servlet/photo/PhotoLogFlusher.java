/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoLogFlusher.java,v 1.4 1999/10/12 22:54:07 dustin Exp $
 */

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;

public class PhotoLogFlusher extends Thread {

	public boolean is_running = false;

	// private static BufferedWriter log_file=null;
	private static PhotoLogger log_object;

	public PhotoLogFlusher(ThreadGroup t) {
		super(t, "log_flusher");
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
					PhotoLogEntry l = null;
					try {
						l = (PhotoLogEntry)v.elementAt(i);
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

	public void run() {
		is_running = true;

		log_object = new PhotoLogger();

		// System.out.println("Running thread...");

		for(;;) {
			try {
				// Wait a second before continuing
				sleep(1000);
			} catch(Exception e) {
			} finally {
				doFlush();

			}
		}
	}

	public void finalize() throws Throwable {
		doFlush();
		super.finalize();
	}
}
