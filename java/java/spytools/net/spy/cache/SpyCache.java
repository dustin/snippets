/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyCache.java,v 1.1 2000/10/17 06:54:58 dustin Exp $
 */

package net.spy.cache;

import java.util.*;

/**
 * Spy in-memory cache object.
 */
public class SpyCache extends Object {
	protected static Hashtable cacheStore=null;
	protected static SpyCacheCleaner cacheCleaner=null;

	/**
	 * Get a new SpyCache object.
	 */
	public SpyCache() {
		super();

		init();
	}

	/**
	 * Store an object in the cache with the specified timeout.
	 *
	 * @param key Cache key
	 * @param value Object to cache
	 * @param cache_time Amount of time (in milliseconds) to store object.
	 */
	public void store(String key, Object value, long cache_time) {
		SpyCacheItem i=new SpyCacheItem(key, value, cache_time);
		synchronized(cacheStore) {
			cacheStore.put(key, i);
		}
	}

	/**
	 * Get an object from the cache, returns null if there's not a valid
	 * object in the cache with this key.
	 *
	 * @param key key of the object to return
	 * @return the object, else null
	 */
	public Object get(String key) {
		Object ret=null;
		synchronized(cacheStore) {
			SpyCacheItem i=(SpyCacheItem)cacheStore.get(key);
			if(i!=null && (!i.expired())) {
				ret=i.getObject();
			}
		}
		return(ret);
	}

	/**
	 * Manually remove an object from the cache.
	 *
	 * @param key key to remove
	 */
	public void uncache(String key) {
		synchronized(cacheStore) {
			cacheStore.remove(key);
		}
	}

	/**
	 * Remove all objects from the cache that begin with the passed in
	 * string.
	 *
	 * @param keystart string to match in the key name
	 */
	public void uncacheLike(String keystart) {
		synchronized(cacheStore) {
			for(Enumeration e=cacheStore.keys(); e.hasMoreElements(); ) {
				String key=(String)e.nextElement();

				// If this matches, kill it.
				if(key.startsWith(keystart)) {
					cacheStore.remove(key);
				}
			} // for loop
		} // lock
	}

	protected synchronized void init() {
		if(cacheStore==null) {
			cacheStore=new Hashtable();
		}

		// start or restart the cache cleaner if needed.
		if(cacheCleaner==null || (!cacheCleaner.isAlive())) {
			cacheCleaner=new SpyCacheCleaner(cacheStore);
		}
	}



	// Private classes
	private class SpyCacheCleaner extends Thread {
		protected Hashtable cacheStore=null;

		// How many cleaning passes we've done.
		protected int passes=0;

		public SpyCacheCleaner(Hashtable cacheStore) {
			super();
			this.cacheStore=cacheStore;
			this.setName("SpyCacheCleaner");
			this.setDaemon(true);
			this.start();
		}

		public String toString() {
			return(super.toString() + " - " + passes + " runs");
		}

		protected void cleanup() throws Exception {
			long now=System.currentTimeMillis();
			synchronized(cacheStore) {
				for(Enumeration e=cacheStore.elements(); e.hasMoreElements();){
					SpyCacheItem i=(SpyCacheItem)e.nextElement();
					if(i.expired()) {
						cacheStore.remove(i.getKey());
					}
				}
			}
			passes++;
		}

		public void run() {
			// Give it ten passes
			while(passes<12) {
				try {
					sleep(300*1000);
					cleanup();
				} catch(Exception e) {
					// Just try again.
				}
			}
		}
	} // Cleaner class


	private class SpyCacheItem extends Object {
		protected Object key=null;
		protected Object value=null;
		protected long exptime=0;

		public SpyCacheItem(Object key, Object value, long cache_time) {
			super();

			this.key=key;
			this.value=value;
			long t=System.currentTimeMillis();
			exptime=t+cache_time;
		}

		public String toString() {
			String out="Cached item:  " + key;
			if(exptime>0) {
				out+="\nExpires:  " + new java.util.Date(exptime);
			}
			out+="\n";
			return(out);
		}

		public Object getObject() {
			return(value);
		}

		public Object getKey() {
			return(key);
		}

		public boolean expired() {
			boolean ret=false;
			if(exptime>0) {
				long t=System.currentTimeMillis();
				ret=(t>exptime);
			}
			return(ret);
		}
	} // SpyCacheStore

}
