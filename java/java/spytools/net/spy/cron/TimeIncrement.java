// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TimeIncrement.java,v 1.1 2001/04/03 07:37:27 dustin Exp $

package net.spy.cron;

import java.util.*;

/**
 * This object defines how a recurring job should increment itself.
 */
public class TimeIncrement extends Object {

	private int field_increment=0;
	private int inc_amount=0;
	private Calendar calendar=null;

	/**
	 * Get a new TimeIncrement object.
	 */
	public TimeIncrement() {
		this(Calendar.getInstance());
	}

	/**
	 * Get a new TimeIncrement object using a specific Calendar.
	 */
	public TimeIncrement(Calendar c) {
		super();
		calendar=c;
	}

	/**
	 * Set Calendar field to increment.
	 */
	public void setField(int to) {
		field_increment=to;
	}

	/**
	 * Set the increment amount.
	 */
	public void setIncrement(int to) {
		inc_amount=to;
	}

	/**
	 * Get the next value of this Date.
	 */
	public Date nextDate(Date d) {
		Date rv=d;

		// Keep incrementing until we've got the thing into the future.
		long now=System.currentTimeMillis();
		while(rv.getTime()<now) {
			calendar.setTime(rv);
			calendar.add(field_increment, inc_amount);
			rv=calendar.getTime();
		}

		System.err.println("Old time:  " + d + " new time " + rv);
		return(rv);
	}

}
