/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLogFlusher.java,v 1.5 2000/01/24 10:11:07 dustin Exp $
 */

package net.spy;

import java.sql.*;
import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * SpyLogFlusher does the actual work of SpyLog.  This is where the log
 * queue is flushed and placed in permanent storage.
 * <p>
 * By default, log entries are written to the file described in the
 * variable <code>logfile</code>.
 * <p>
 * Classes overriding this will be most interested in the protected method
 * doFlush().  doFlush is called on a given interval when it's time to
 * flush the logs.  The logs are available via a SpyLog object called
 * ``log_object.''  To get a list of all log entries that need to be
 * flushed, the following piece of code may be executed:<br>
 * <code>Vector v = log_object.flush();</code>
 */

public class SpyLogFlusher extends Thread {

	// private static BufferedWriter log_file=null;
	protected static SpyLog log_object;

	/**
	 * Path to the logfile.  The default logfile is /tmp/spy.log, but this
	 * can be overridden.
	 */
	public String logfile = "/tmp/spy.log";

	/**
	 * Get a SpyFlusher and place it in a given threadgroup.
	 *
	 * @param t the threadgroup in which the SpyFlusher should be placed.
	 */
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

	protected void finalize() throws Throwable {
		doFlush();
		super.finalize();
	}
}
