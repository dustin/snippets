/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLogFlusher.java,v 1.8 2001/02/07 06:31:26 dustin Exp $
 */

package net.spy.log;

import net.spy.*;

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
	private SpyLogQueue log_queue=null;

	/**
	 * Path to the logfile.  The default logfile is /tmp/spy.log, but this
	 * can be overridden.
	 */
	public String logfile = "/tmp/spy.log";
	private String queue_name=null;

	/**
	 * Get a SpyFlusher for the given queue.
	 */
	public SpyLogFlusher(String name) {
		super();
		setDaemon(true);
		setName("SpyLogFlusher-" + name);
		this.queue_name=name;
		// Exception e=new Exception("Instantiated SpyLogFlusher-" + name);
		// e.printStackTrace();
		configure();
	}

	/**
	 * Get a SpyFlusher for the given queue in the given ThreadGroup
	 */
	public SpyLogFlusher(String name, ThreadGroup t) {
		super(t, "SpyLogFlusher");
		setDaemon(true);
		setName("SpyLogFlusher-" + name);
		this.queue_name=name;
		// Exception e=new Exception("Instantiated SpyLogFlusher-" + name);
		// e.printStackTrace();
		configure();
	}

	/**
	 * Get a SpyFlusher.
	 * @deprecated Please provide a queue name
	 */
	public SpyLogFlusher() {
		this("GenericSpyLogQueue");
		Exception e=new Exception("Using deprecated API!");
		System.err.println("Warning!  " + e);
		e.printStackTrace();
	}

	/**
	 * Get a SpyFlusher and place it in a given threadgroup.
	 *
	 * @param t the threadgroup in which the SpyFlusher should be placed.
	 * @deprecated Please provide a queue name
	 */
	public SpyLogFlusher(ThreadGroup t) {
		this("GenericSpyLogQueue", t);
		Exception e=new Exception("Using deprecated API!");
		System.err.println("Warning!  " + e);
		e.printStackTrace();
	}

	/**
	 * Do additional configuration stuff here.
	 */
	protected void configure() {
	}

	/**
	 * Return the current queue of things to be logged
	 */
	protected Vector flush() {
		return(log_queue.flush());
	}

	protected void doFlush() {
		Vector v = flush();
		// Only do all this crap if there's something to log.
		if(v.size() > 0) {
			// The logfile is only open long enough for us to write our log
			// entries to it.
			BufferedWriter log_file=null;
			try {
				log_file=new BufferedWriter(
					new FileWriter(logfile, true));
				for(int i = 0; i<v.size(); i++) {
					SpyLogEntry l = (SpyLogEntry)v.elementAt(i);
					log_file.write(l.toString() + "\n");
				}
				log_file.flush();
				log_file.close(); // Close it, we're done!
			} catch(Exception e) {
				System.err.println("BAD LOG ERRROR!  " + e);
			}
		}
	}

	public void run() {
		log_queue = new SpyLogQueue(queue_name);

		for(;;) {
			try {
				// Flush first, ask questions later.
				doFlush();

				// Wait for something to get added...with timeout
				log_queue.waitForQueue(60000);

			} catch(Exception e) {
				System.err.println("Error flushing logs:  " + e);
				e.printStackTrace();
			}
		}
	}

	protected void finalize() throws Throwable {
		doFlush();
		super.finalize();
	}
}
