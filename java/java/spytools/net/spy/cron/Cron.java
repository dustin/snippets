// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cron.java,v 1.3 2001/04/03 07:59:25 dustin Exp $

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
			System.err.println("Looking for jobs to run at " + new Date());
			for(Enumeration e=jq.getReadyJobs(); e.hasMoreElements(); ) {
				Job j=(Job)e.nextElement();
				System.err.println("Starting job " + j);
				Thread t=new Thread(j);
				t.setName(j.getName());
				t.setDaemon(true);
				t.start();
			}
			System.err.println("Done looking for jobs to run.");

			try {
				// Check once a minute
				// sleep(60000);
				sleep(10000);
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
