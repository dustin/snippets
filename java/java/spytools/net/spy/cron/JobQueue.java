// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: JobQueue.java,v 1.4 2001/04/06 09:09:05 dustin Exp $

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

	/**
	 * Get the time the next job will start.
	 */
	public Date getNextStartDate() {
		Date next=null;
		long soonestJob=86400*1000;
		long now=System.currentTimeMillis();
		for(Enumeration e=elements(); e.hasMoreElements(); ) {
			Job j=(Job)e.nextElement();
			Date jdate=j.getStartTime();
			if(jdate!=null) {
				long t=jdate.getTime()-now;
				if(t>0 && t<soonestJob) {
					soonestJob=t;
					next=jdate;
				}
			}
		}

		return(next);
	}

}
