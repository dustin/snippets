// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cron.java,v 1.10 2002/08/21 05:22:42 dustin Exp $

package net.spy.cron;

import java.io.File;

import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;

/**
 * Watches a JobQueue and invokes the Jobs when they're ready.
 */
public class Cron extends Thread {

	private JobQueue jq=null;
	private boolean stillRunning=true;

	// How long we can go idle.
	private long maxIdleTime=900000;
	// Time we last saw a valid job
	private long validJobFound=0;

	/**
	 * Get a new Cron instance operating on the given queue.
	 *
	 * @param jq the job queue to watch
	 */
	public Cron(JobQueue jq) {
		this("Cron", jq);
	}

	/**
	 * Get a new Cron object operating on the given queue.
	 *
	 * @param name thread name
	 * @param jq job queue to watch
	 */
	public Cron(String name, JobQueue jq) {
		super();
		this.jq=jq;
		setDaemon(true);
		setName(name);
		start();
		validJobFound=System.currentTimeMillis();
	}

	/**
	 * Get the current job queue.
	 */
	public JobQueue getJobQueue() {
		return(jq);
	}

	/**
	 * Shut down the queue.
	 */
	public void shutdown() {
		stillRunning=false;
		synchronized(jq) {
			jq.notify();
		}
	}

	public void run() {
		while(stillRunning) {
			// Check all the running jobs.
			for(Iterator i=jq.getReadyJobs(); i.hasNext(); ) {
				Job j=(Job)i.next();
				debug("Starting job " + j);
				Thread t=new Thread(j);
				t.setName(j.getName());
				t.setDaemon(true);
				t.start();
			}

			// Just to slow things down a bit.
			yield();

			// Find the soonest job less than a day out.
			long now=System.currentTimeMillis();
			Date next=jq.getNextStartDate();;
			long soonestJob=0;

			// If we didn't get a next job start date, the queue is likely
			// empty.  If we shut down on an empty queue, shut down.
			if(next==null) {
				// If it's been too long, shut down
				if( (now-validJobFound) > maxIdleTime) {
					debug("Been a long time "
						+ "since I had a job.  Shutting down.");
					shutdown();
				}
				soonestJob=60000;
			} else {
				soonestJob=next.getTime()-now;
				validJobFound=now;
			}

			try {
				if(next!=null) {
					debug("Sleeping "
						+ soonestJob + "ms (next job at " + next + ").");
				} else {
					debug("Sleeping "
						+ soonestJob + "ms (no good date found).");
				}
				// If we're still running at this point, wait for a job
				if(stillRunning) {
					// Take a second off of the sleep (will use it later)
					soonestJob-=1000;
					// Make sure it's greater than 1, or it'll sleep forever
					if(soonestJob < 1) {
						soonestJob = 1;
					}
					debug("Sleeping for " + soonestJob);
					// Sleep on the job
					synchronized(jq) {
						jq.wait(soonestJob);
					}
					// Wait the remaining second
					sleep(1000);
					debug("Finished sleep.");
				} else {
					debug("Not sleeping, game over.");
				}
			} catch(Exception e) {
				// Don't care, flip faster
				e.printStackTrace();
			}
		} // still running
		debug("shut down");
	}

	private void debug(String msg) {
		System.err.println("CRON:  " + msg);
	}

	/**
	 * Set the maximum amount of time the cron thread will continue running
	 * with no jobs.
	 *
	 * @param maxIdleTime maximum amount of time in milliseconds
	 */
	public void setMaxIdleTime(long maxIdleTime) {
		this.maxIdleTime=maxIdleTime;
	}

	/**
	 * Run a Cron instance against a FileJobQueue.
	 */
	public static void main(String args[]) throws Exception {
		FileJobQueue jq=new FileJobQueue(new File(args[0]));
		Cron c=new Cron(jq);

		// Get the time incrementor.
		TimeIncrement ti=new TimeIncrement();
		ti.setField(Calendar.MINUTE);
		ti.setIncrement(2);

		while(c.isAlive()) {
			sleep(10000);
		}
	}
}
