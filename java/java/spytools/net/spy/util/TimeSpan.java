// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TimeSpan.java,v 1.1 2001/11/13 23:35:45 dustin Exp $

package net.spy.util;

import java.util.*;

/**
 * Creates a representable timespan.
 */
public class TimeSpan extends Object {

	// From
	private Date from=null;
	// To
	private Date to=null;

	long days=0;
	long hours=0;
	long minutes=0;
	long seconds=0;
	long mseconds=0;

	boolean future=false;

	/**
	 * Get an instance of TimeSpan representing the span between the given
	 * from and to dates.
	 */
	public TimeSpan(Date from, Date to) {
		super();
		this.from=from;
		this.to=to;

		long diff=Math.abs(to.getTime()-from.getTime());

		if(diff<=0) {
			future=false;
		}

		// Days
		if(diff>86400000l) {
			days=diff/864000000l;
			diff=diff%86400000l;
		}

		// Hours
		if(diff>3600000l) {
			hours=diff/3600000l;
			diff=diff%3600000l;
		}

		// Minutes
		if(diff>60000l) {
			minutes=diff/60000l;
			diff=diff%60000l;
		}

		// Seconds
		if(diff>1000l) {
			seconds=diff/1000l;
			diff=diff%1000l;
		}

		mseconds=diff;
	}

	// Provide a two-digit number as a String.
	private String zeroNumber(long in) {
		StringBuffer sb=new StringBuffer();
		if(in<10) {
			sb.append("0");
		}
		sb.append(in);
		return(sb.toString());
	}

	/**
	 * Print out the timespan.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer();
		boolean hasDays=false;

		if(days>0) {
			hasDays=true;
			sb.append(days);
			sb.append(days>1?" days":" day");
		}

		if(hours>0 || minutes>0 || seconds>0 || mseconds>0) {
			if(hasDays) {
				sb.append(" ");
			}
			sb.append(zeroNumber(hours));
			sb.append(":");
			sb.append(zeroNumber(minutes));
			sb.append(":");
			sb.append(zeroNumber(seconds));
			sb.append(".");
			sb.append(mseconds);
		}

		return(sb.toString());
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		java.text.SimpleDateFormat sdf=new java.text.SimpleDateFormat(
			"yyyy-MM-dd HH:mm:ss");
		TimeSpan ts=new TimeSpan(sdf.parse(args[0]), sdf.parse(args[1]));
		System.out.println(ts);
	}

}
