// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cron.java,v 1.8 2002/08/16 07:26:57 dustin Exp $

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

	/**
	 * Get a new Cron object operating on the given queue.
	 */
	public Cron(JobQueue jq) {
		super();
		this.jq=jq;
		setDaemon(true);
		setName("Cron");
		start();
	}

	/**
	 * Get the current job queue.
	 */
	public JobQueue getJobQueue() {
		return(jq);
	}

	public void run() {
		while(stillRunning) {
			// Check all the running jobs.
			for(Iterator i=jq.getReadyJobs(); i.hasNext(); ) {
				Job j=(Job)i.next();
				System.err.println("CRON: Starting job " + j);
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

			// If we didn't get a next job start date, sleep a second.
			if(next==null) {
				soonestJob=1000;
			} else {
				soonestJob=next.getTime()-now;
			}

			try {
				if(next!=null) {
					System.err.println("CRON: Sleeping "
						+ soonestJob + "ms (next job at " + next + ").");
				} else {
					System.err.println("CRON: Sleeping "
						+ soonestJob + "ms (no good date found).");
				}
				synchronized(jq) {
					jq.wait(soonestJob);
				}
			} catch(Exception e) {
				// Don't care, flip faster
			}
		}
	}

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
