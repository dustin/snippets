// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Job.java,v 1.6 2002/08/21 07:08:59 dustin Exp $

package net.spy.cron;

import java.util.Date;

/**
 * All Jobs should implement this interface.
 */
public abstract class Job extends Object implements Runnable {

	// The next time the job is due to start.
	private Date nextStart=null;
	// How to increment the time value in the case of recurring jobs.
	private TimeIncrement ti=null;
	// Whether or not this job has run.
	private boolean hasrun=false;
	// Whether the job is currently running or not
	private boolean isrunning=false;
	// The name of ths thing
	private String name=null;

	/**
	 * Get a new Job with the given name and start date.
	 */
	public Job(String name, Date startDate) {
		this(name, startDate, null);
	}

	/**
	 * Get a new recurring Job with the given name and start date that will
	 * run on an interval defined by the TimeIncrement.
	 */
	public Job(String name, Date startDate, TimeIncrement ti) {
		super();
		nextStart=startDate;
		this.ti=ti;
		setName(name);
	}

	/**
	 * Get a string representation of this Job.
	 */
	public String toString() {
		return("Job:" + getName());
	}

	/**
	 * Set the name of this thing.
	 */
	public void setName(String to) {
		name=to;
	}

	/**
	 * Get the name of this thing.
	 */
	public String getName() {
		return(name);
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
	 * Mark started, call runJob, then mark finished.
	 */
	public final void run() {
		markStarted();
		runJob();
		markFinished();
	}

	/**
	 * Subclasses of Job should extend this method to implement their
	 * running.
	 */
	protected abstract void runJob();

	/**
	 * Is this Job ready to be thrown away?
	 */
	public boolean isTrash() {
		// When nextStart is null, we won't be starting again.
		return(nextStart==null);
	}

	/**
	 * Get the current TimeIncrement object incrementing the time on this job.
	 *
	 * @return the TimeIncrement
	 */
	protected TimeIncrement getTimeIncrement() {
		return(ti);
	}

	/**
	 * Mark this job as having been started.
	 */
	protected synchronized void markStarted() {
		hasrun=true;
		if(ti==null) {
			nextStart=null;
		} else {
			nextStart=ti.nextDate(nextStart);
		}
		isrunning=true;
	}

	/**
	 * Stop this job from running.
	 */
	protected void stopRunning() {
		nextStart=null;
		ti=null;
	}

	/**
	 * Is this job alive (is it running)?
	 */
	public boolean isAlive() {
		return(isrunning);
	}

	/**
	 * Mark this job as having stopped running.
	 */
	protected void markFinished() {
		isrunning=false;
		// Not sure, but if I don't slow down here, I start running into
		// problems.
		Thread.yield();
	}
}
