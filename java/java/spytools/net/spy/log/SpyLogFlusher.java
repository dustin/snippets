/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyLogFlusher.java,v 1.12 2001/07/03 05:04:21 dustin Exp $
 */

package net.spy.log;

import net.spy.*;

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
	boolean keep_going=true;

	/**
	 * Path to the logfile.  The default logfile is /tmp/spy.log, but this
	 * can be overridden.
	 */
	public String logfile = "/tmp/spy.log";
	private String queue_name=null;

	private Date lastRun=null;
	private Date lastErrorTime=null;
	private Exception lastError=null;

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
	 * Get a string describing this thingy.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer(super.toString());
		sb.append(" - ");
		sb.append(queueSize());
		sb.append(" items queued");
		if(lastRun!=null) {
			sb.append(", last run:  ");
			sb.append(lastRun);
		}
		if(lastError!=null) {
			sb.append(", last error:  ");
			sb.append(lastError);
			sb.append(" time of last error:  ");
			sb.append(lastErrorTime);
		}
		return(sb.toString());
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

	/**
	 * Get the current size of the queue.
	 */
	public int queueSize() {
		return(log_queue.size());
	}

	/**
	 * This method should writes the log entries to their final
	 * destination.  The default implementation writes the log entries to a
	 * file.  This method should probably be overridden to be useful.
	 */
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

				lastRun=new Date();
			} catch(Exception e) {
				System.err.println("BAD LOG ERRROR!  " + e);
				lastError=e;
				lastErrorTime=new Date();
			}
		}
	}

	/**
	 * Stop was taken and deprecated by those fools at Javasoft.  I use
	 * close here because it's kinda the right thing to do, as we're
	 * closing the log...sorta.
	 */
	public void close() {
		keep_going=false;
	}

	/**
	 * Periodically process the log queue.  You probably don't want to
	 * override this.
	 */
	public void run() {
		log_queue = new SpyLogQueue(queue_name);

		while(keep_going) {
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
