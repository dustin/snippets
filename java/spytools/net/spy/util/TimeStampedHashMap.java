// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TimeStampedHashMap.java,v 1.1 2002/08/16 07:27:12 dustin Exp $

package net.spy.util;

import java.util.HashMap;

/**
 * A HashMap that remembers when it was accessed.  Good for caching and
 * stuff like that.
 */
public class TimeStampedHashMap extends HashMap {

	// Where we keep up with usage.
	private long timestamp=0;
	private long lastPut=0;
	private long lastGet=0;
	private long hits=0;
	private long misses=0;
	private long watermark=0;
	private long puts=0;

	/**
	 * Get an instance of TimeStampedHashMap.
	 */
	public TimeStampedHashMap() {
		super();
		long now=System.currentTimeMillis();
		timestamp=now;
		lastPut=now;
		lastGet=now;
	}

	/**
	 * Get an object from the map.
	 *
	 * @param key the object to get
	 * @return the object, or null if it's missing
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
	 * Put an object into the map.
	 *
	 * @param key the key to store
	 * @param value the value to store
	 * @return the object that got displaced for this store, or null
	 */
	public Object put(Object key, Object value) {
		Object rv=super.put(key, value);
		markPut();
		return(rv);
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
