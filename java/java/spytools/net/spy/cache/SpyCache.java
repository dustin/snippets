/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyCache.java,v 1.9 2001/07/28 02:39:13 dustin Exp $
 */

package net.spy.cache;

import java.util.*;
import java.net.*;
import java.io.*;

import net.spy.util.*;

/**
 * Spy in-memory cache object.
 *
 * <p>
 *
 * If the system properties <tt>net.spy.cache.multi.addr</tt> and
 * <tt>net.spy.cache.multi.port</tt> are both set, requests may be sent as
 * ASCII strings on that multicast group and port to clear cache entries
 * based on prefix.
 */
public class SpyCache extends Object {
	private static TimeStampedHash cacheStore=null;
	private static String CACHE_MUTEX="CACHE_MUTEX";
	private static SpyCacheCleaner cacheCleaner=null;
	private static String CLEANER_MUTEX="CLEANER_MUTEX";

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

	private synchronized void init() {
		synchronized(CACHE_MUTEX) {
			if(cacheStore==null) {
				cacheStore=new TimeStampedHash();
			}
		}

		// start or restart the cache cleaner if needed.
		synchronized(CLEANER_MUTEX) {
			if(cacheCleaner==null || (!cacheCleaner.isAlive())) {
				cacheCleaner=new SpyCacheCleaner(cacheStore);
			}
		}
	}

	////////////////////////////////////////////////////////////////////
	//                       Private Classes                          //
	////////////////////////////////////////////////////////////////////

	private class SpyCacheCleaner extends Thread {
		private TimeStampedHash cacheStore=null;

		// How many cleaning passes we've done.
		private int passes=0;

		// This is so we can only report multicast security exceptions
		// once.
		private boolean reportedMulticastSE=false;

		// Insert multicast listener here.
		private CacheClearRequestListener listener=null;

		public SpyCacheCleaner(TimeStampedHash cacheStore) {
			super();
			this.cacheStore=cacheStore;
			this.setName("SpyCacheCleaner");
			this.setDaemon(true);
			this.start();
		}

		public String toString() {
			return(super.toString() + " - " + passes + " runs, mod age:  "
				+ cacheStore.getUseAge()
				+ ", items cached:  " + cacheStore.size());
		}

		private void cleanup() throws Exception {
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

		private boolean shouldIContinue() {
			boolean rv=false;

			// Return true if the difference between now and the last
			// time the cache was touched is less than an hour.
			if( (cacheStore.getUseAge()) < (3600*1000) ) {
				rv=true;
			}

			return(rv);
		}

		// Make sure our multicast listener is still running if it should be.
		private void checkMulticastThread() {
			try {
				String addr_s=System.getProperty("net.spy.cache.multi.addr");
				String port_s=System.getProperty("net.spy.cache.multi.port");

				if(addr_s!=null && port_s!=null) {
					int port=Integer.parseInt(port_s);

					InetAddress group = InetAddress.getByName(addr_s);
					listener=new CacheClearRequestListener(group, port);
				}

			} catch(SecurityException se) {
				// Only do this the first time.
				if(!reportedMulticastSE) {
					se.printStackTrace();
					reportedMulticastSE=true;
				}
			} catch(IOException ioe) {
				ioe.printStackTrace();
			}
		}

		public void run() {

			boolean keepgoing=true;

			// It will keep going until nothing's been touched in the cache
			// for an hour, at which point it'll dump the whole cache and join.
			while(keepgoing) {
				try {
					if(listener==null || (!listener.isAlive())) {
						checkMulticastThread();
					}
					sleep(300*1000);
					cleanup();
				} catch(Exception e) {
					// Just try again.
				}

				keepgoing=shouldIContinue();
			}

			// OK, we're about to bail, let's dump the cache and go.
			cacheStore.clear();

			// Tell the multicast listener to stop if we have one
			if(listener!=null) {
				listener.stopRunning();
			}
		}
	} // Cleaner class

	private class SpyCacheItem extends Object {
		private Object key=null;
		private Object value=null;
		private long exptime=0;

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
	} // SpyCacheItem

}
