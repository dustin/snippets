// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Job.java,v 1.1 2001/04/02 08:40:25 dustin Exp $

package net.spy.cron;

import java.util.*;

/**
 * All Jobs should implement this interface.
 */
public abstract class Job extends Thread {

	// The next time the job is due to start.
	private Date nextStart=null;

	public Job(String name) {
		super(name);
	}

	/**
	 * Get the time this job was requested to start.
	 */
	public Date getStartTime() {
		return(nextStart);
	}

	/**
	 * Set the next time the job is due to start.
	 */
	public void setStartTime(Date to) {
		nextStart=to;
	}

	/**
	 * Is this Job ready to go?
	 */
	public boolean isReady() {
		boolean rv=false;

		// Short circuit a null nextStart Date.
		if(nextStart==null) {
			return(false);
		}

		long now=System.currentTimeMillis();

		// If the time is current or has passed, and the job is not
		// currently running, then it's ready to run.
		if( (nextStart.getTime() <= now) && (!isAlive()) ) {
			rv=true;
		}

		return(rv);
	}

	/**
	 * Mark this job as having been started.
	 */
	public void markStarted() {
		nextStart=null;
	}

}
