// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyCacheStore.java,v 1.1 2001/05/21 23:05:45 dustin Exp $

package net.spy.cache;

import java.util.*;

/**
 * Where cached data are stored.
 */
public class SpyCacheStore extends Hashtable {

	// Where we keep up with usage.
	private long timestamp=0;

	/**
	 * Get an instance of SpyCacheStore.
	 */
	public SpyCacheStore() {
		super();
		timestamp();
	}

	/**
	 * @see Hashtable
	 */
	public Object get(Object key) {
		timestamp();
		return(super.get(key));
	}

	/**
	 * @see Hashtable
	 */
	public Object put(Object key, Object value) {
		timestamp();
		return(super.put(key, value));
	}

	// Mark the timestamp.
	private void timestamp() {
		timestamp=System.currentTimeMillis();
	}

	/**
	 * Find out the last time this thing was used (put or get was called).
	 */
	public long getTimestamp() {
		return(timestamp);
	}

	/**
	 * How many milliseconds ago was the last put or get operation?
	 */
	public long getModAge() {
		long now=System.currentTimeMillis();
		return(now-timestamp);
	}

}
