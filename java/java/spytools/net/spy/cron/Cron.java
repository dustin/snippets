// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cron.java,v 1.1 2001/04/02 08:40:23 dustin Exp $

package net.spy.cron;

import java.util.*;

public class Cron extends Thread {

	private JobQueue jq=null;
	private boolean stillRunning=true;

	/**
	 * Get a new Cron object operating on the given queue.
	 */
	public Cron(JobQueue jq) {
		super();
		setDaemon(true);
		setName("Cron");
		start();
	}

	public void run() {
		while(stillRunning) {
			// Check all the running jobs.
			for(Enumeration e=jq.getReadyJobs(); e.hasMoreElements(); ) {
				Job j=(Job)e.nextElement();
				System.err.println("Starting job " + j);
				j.markStarted();
				j.start();
			}

			try {
				// Check once a minute
				sleep(60000);
			} catch(Exception e) {
				// Don't care, flip faster
			}
		}
	}

	public static void main(String args[]) throws Exception {
		JobQueue jq=new JobQueue();
		Cron c=new Cron(jq);
		// In this case, Cron will not be a daemon thread, as it's the only
		// thing main will be running.
		c.setDaemon(false);
	}

}
