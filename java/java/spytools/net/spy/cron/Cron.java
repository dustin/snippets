// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cron.java,v 1.5 2001/04/04 09:15:19 dustin Exp $

package net.spy.cron;

import java.util.*;
import java.io.*;

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
			for(Enumeration e=jq.getReadyJobs(); e.hasMoreElements(); ) {
				Job j=(Job)e.nextElement();
				System.err.println("CRON: Starting job " + j);
				Thread t=new Thread(j);
				t.setName(j.getName());
				t.setDaemon(true);
				t.start();
			}

			// Just to slow things down a bit.
			yield();

			// Find the soonest job less than a day out.
			long soonestJob=86400*1000;
			long now=System.currentTimeMillis();
			Date next=null;
			// Figure out how long we need to sleep.
			for(Enumeration e=jq.elements(); e.hasMoreElements(); ) {
				Job j=(Job)e.nextElement();
				long t=j.getStartTime().getTime()-now;
				if(t>0 && t<soonestJob) {
					soonestJob=t;
					next=j.getStartTime();
				}
			}

			// If we didn't get a next job start date, sleep a second.
			if(next==null) {
				soonestJob=1000;
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
