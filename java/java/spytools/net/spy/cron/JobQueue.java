// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: JobQueue.java,v 1.6 2002/08/16 07:26:58 dustin Exp $

package net.spy.cron;

import java.util.Date;
import java.util.Iterator;
import java.util.ArrayList;

/**
 * This is where all the jobs go.
 */
public class JobQueue extends ArrayList {

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
		add(j);
		synchronized(this) {
			notifyAll();
		}
	}

	/**
	 * Get an Iterator of Jobs that are ready to run.
	 */
	public synchronized Iterator getReadyJobs() {
		ArrayList v=new ArrayList();

		// Flip through all of the jobs and see what we've got to do.
		for(Iterator i=iterator(); i.hasNext(); ) {
			Job j=(Job)i.next();

			// Add a job if it's ready.
			if(j.isReady()) {
				v.add(j);
			} else if(j.isTrash()) {
				i.remove();
			}
		}

		return(v.iterator());
	}

	/**
	 * Get the time the next job will start.
	 */
	public Date getNextStartDate() {
		Date next=null;
		long soonestJob=86400*1000;
		long now=System.currentTimeMillis();
		for(Iterator i=iterator(); i.hasNext(); ) {
			Job j=(Job)i.next();
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
