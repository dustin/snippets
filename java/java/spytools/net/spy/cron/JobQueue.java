// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: JobQueue.java,v 1.3 2001/04/04 08:45:19 dustin Exp $

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
	public synchronized void addJob(Job j) {
		addElement(j);
		synchronized(this) {
			notifyAll();
		}
	}

	/**
	 * Get an Enumeration of Jobs that are ready to run.
	 */
	public synchronized Enumeration getReadyJobs() {
		Vector v=new Vector();

		// Flip through all of the jobs and see what we've got to do.
		for(Enumeration e=elements(); e.hasMoreElements(); ) {
			Job j=(Job)e.nextElement();

			// Add a job if it's ready.
			if(j.isReady()) {
				v.addElement(j);
			} else if(j.isTrash()) {
				v.removeElement(j);
			}
		}

		return(v.elements());
	}

}
