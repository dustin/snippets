/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLogFlusher.java,v 1.3 1999/12/15 03:58:15 dustin Exp $
 */

package net.spy;

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;

public class SpyLogFlusher extends Thread {

	// private static BufferedWriter log_file=null;
	protected static SpyLog log_object;

	public String logfile = "/tmp/spy.log";

	public SpyLogFlusher(ThreadGroup t) {
		super(t, "log_flusher");
		this.setDaemon(true);
	}

	protected synchronized void doFlush() {
		Vector v = log_object.flush();
		// Only do all this crap if there's something to log.
		if(v.size() > 0) {
			BufferedWriter log_file=null;
			try {
				log_file=new BufferedWriter(
					new FileWriter(logfile, true));
				for(int i = 0; i<v.size(); i++) {
					SpyLogEntry l = (SpyLogEntry)v.elementAt(i);
					log_file.write(l.toString() + "\n");
				}
				log_file.flush();
			} catch(Exception e) {
				System.err.println("BAD LOG ERRROR!  " + e);
			}
		}
	}

	public void run() {
		log_object = new SpyLog();

		for(;;) {
			try {
				// Wait five seconds before continuing
				sleep(5000);
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
