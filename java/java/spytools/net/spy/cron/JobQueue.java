// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: JobQueue.java,v 1.1 2001/04/02 08:40:27 dustin Exp $

package net.spy.cron;

import java.util.*;

/**
 * This is where all the jobs go.
 */
public class JobQueue extends Vector {

	/**
	 * Get a new job queue.
	 */
	public JobQueue() {
		super();
	}

	/**
	 * Add a job.
	 */
	public void addJob(Job j) {
		addElement(j);
	}

	/**
	 * Get an Enumeration of Jobs that are ready to run.
	 */
	public Enumeration getReadyJobs() {
		Vector v=new Vector();

		// Find all the jobs that should have been started by now.
		for(Enumeration e=elements(); e.hasMoreElements(); ) {
			Job j=(Job)e.nextElement();

			// Add a job if it's ready.
			if(j.isReady()) {
				v.addElement(j);
			}
		}

		return(v.elements());
	}

}
