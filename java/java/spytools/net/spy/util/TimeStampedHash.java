// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TimeStampedHash.java,v 1.1 2001/05/22 03:28:17 dustin Exp $

package net.spy.util;

import java.util.*;

/**
 * A Hashtable that remembers when it was accessed.  Good for caching and
 * stuff like that.
 */
public class TimeStampedHash extends Hashtable {

	// Where we keep up with usage.
	private long timestamp=0;
	private long lastPut=0;
	private long lastGet=0;

	/**
	 * Get an instance of SpyCacheStore.
	 */
	public TimeStampedHash() {
		super();
		long now=System.currentTimeMillis();
		timestamp=now;
		lastPut=now;
		lastGet=now;
	}

	/**
	 * @see Hashtable
	 */
	public Object get(Object key) {
		markGet();
		return(super.get(key));
	}

	/**
	 * @see Hashtable
	 */
	public Object put(Object key, Object value) {
		markPut();
		return(super.put(key, value));
	}

	// Mark a get
	private void markPut() {
		lastPut=System.currentTimeMillis();
		timestamp=lastPut;
	}

	// Mark a get
	private void markGet() {
		lastGet=System.currentTimeMillis();
		timestamp=lastGet;
	}

	/**
	 * Find out the last time this thing was used (put or get was called).
	 */
	public long getTimestamp() {
		return(timestamp);
	}

	/**
	 * Find out the last time this thing last had a Get.
	 */
	public long getLastGet() {
		return(lastGet);
	}

	/**
	 * Find out the last time this thing last had a Put.
	 */
	public long getLastPut() {
		return(lastPut);
	}

	/**
	 * How many milliseconds ago was the last put operation?
	 */
	public long getPutAge() {
		long now=System.currentTimeMillis();
		return(now-lastPut);
	}

	/**
	 * How many milliseconds ago was the last get operation?
	 */
	public long getGetAge() {
		long now=System.currentTimeMillis();
		return(now-lastGet);
	}

	/**
	 * How many milliseconds ago was the last put or get operation?
	 */
	public long getUseAge() {
		long now=System.currentTimeMillis();
		return(now-timestamp);
	}

}
