// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: DBTTL.java,v 1.3 2002/07/10 04:25:21 dustin Exp $

package net.spy.db;

/**
 * Used to track checked out DB connections to report on connections that
 * have been checked out longer than we expect them to be.
 */
public class DBTTL extends Object {

	private long ttl=0;
	private long startTime=0;
	private Exception e=null;
	private Object extraInfo=null;

	private boolean isClosed=false;
	private long lastReport=0;
	private int nReports=0;

	private static final long REPORT_INTERVAL=300000;
	private static final long N_REPORTS=10;

	/**
	 * Get an instance of DBTTL.
	 */
	public DBTTL(long ttl) {
		super();
		this.ttl=ttl;
		this.startTime=System.currentTimeMillis();
		this.e=new Exception("DB TTL Expired");
	}

	/**
	 * String me.
	 */
	public String toString() {
		return("DBTTL:  " + ttl);
	}

	/**
	 * Provide extra information for the TTL report.
	 */
	public void setExtraInfo(Object o) {
		this.extraInfo=o;
	}

	/**
	 * Calling this method states that we are no longer interested in the
	 * progress of this TTL.
	 */
	public void close() {
		isClosed=true;
	}

	/**
	 * Return true if this TTL is no longer interesting.
	 */
	public boolean isClosed() {
		return(isClosed);
	}

	private boolean isExpired() {
		boolean rv=false;
		long now=System.currentTimeMillis();

		if(!isClosed()) {
			if(now > startTime+ttl) {
				rv=true;
			}
		}

		return(rv);
	}

	// Has it been long enough since our last report?
	private boolean readyForReport() {
		long now=System.currentTimeMillis();

		return( (now-lastReport) > REPORT_INTERVAL);
	}

	/**
	 * Request a report of the TTL.  This will only print if the TTL has
	 * expired and we haven't shown a report for this particular TTL within
	 * the last REPORT_INTERVAL milliseconds.
	 */
	public void report() {
		if(isExpired() && readyForReport()) {
			long now=System.currentTimeMillis();
			// Basic idea of what's going on.
			System.err.println("This DB query has been running for "
				+ (now-startTime) + "ms with a max expectancy of "
				+ ttl + "ms");
			if(extraInfo!=null) {
				System.err.println(extraInfo);
			}
			e.printStackTrace();
			lastReport=now;

			// If we've reported enough times, give up.
			if(nReports++ >= N_REPORTS) {
				close();
			}
		}
	}

}

