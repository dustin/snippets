// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TimeStampedHash.java,v 1.2 2001/08/28 22:51:51 dustin Exp $

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
	private long hits=0;
	private long misses=0;
	private long watermark=0;
	private long puts=0;

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
		Object o=super.get(key);
		if(o==null) {
			misses++;
		} else {
			hits++;
		}
		return(o);
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

		// Update the stats
		puts++;
		synchronized(this) {
			if(size()>watermark) {
				watermark=size();
			}
		}
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

	/**
	 * Get the number of hits (number of requests for items that were
	 * actually there).
	 */
	public long getHits() {
		return(hits);
	}

	/**
	 * Get the number of misses (number of requests for items that were not
	 * actually there).
	 */
	public long getMisses() {
		return(misses);
	}

	/**
	 * Get the watermark (maximum number of objects seen at any one time).
	 */
	public long getWatermark() {
		return(watermark);
	}

	/**
	 * Get the total number of put() invocations.
	 */
	public long getNumPuts() {
		return(puts);
	}

}
