/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyCache.java,v 1.15 2002/08/08 21:56:32 dustin Exp $
 */

package net.spy.cache;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.ReferenceQueue;

import java.io.IOException;

import java.net.InetAddress;

import java.util.Iterator;

import net.spy.util.TimeStampedHash;

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

	private TimeStampedHash cacheStore=null;
	private SpyCacheCleaner cacheCleaner=null;
	private ReferenceQueue refQueue=null;

	private static SpyCache instance=null;

	// This is a singleton, the constructor should only be called once.
	private SpyCache() {
		super();

		init();
	}

	private void init() {
		cacheStore=new TimeStampedHash();
		refQueue=new ReferenceQueue();
	}

	private void checkThread() {
		if(cacheCleaner==null || (!cacheCleaner.isAlive())) {
			// Do the same thing synchronized
			synchronized(SpyCache.class) {
				if(cacheCleaner==null || (!cacheCleaner.isAlive())) {
					cacheCleaner=new SpyCacheCleaner(cacheStore, refQueue);
				}
			}
		}
	}

	/**
	 * Get the instance of SpyCache.
	 *
	 * @return the instance of SpyCache, or a new instance if required
	 */
	public static SpyCache getInstance() {
		if(instance == null) {
			synchronized(SpyCache.class) {
				if(instance==null) {
					instance=new SpyCache();
				}
			}
		}

		instance.checkThread();

		return(instance);
	}

	/**
	 * Store an object in the cache with the specified timeout.
	 *
	 * @param key Cache key
	 * @param value Object to cache
	 * @param cacheTime Amount of time (in milliseconds) to store object.
	 */
	public void store(String key, Object value, long cacheTime) {
		SpyCacheItem i=new SpyCacheItem(key,
			new SoftReference(value, refQueue), cacheTime);
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
				Reference ref=i.getObject();
				ret=ref.get();
				// If the reference returned a null object, remove this
				// item from the cache.
				if(ret==null) {
					cacheStore.remove(key);
				}
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
			for(Iterator i=cacheStore.keySet().iterator(); i.hasNext(); ) {
				String key=(String)i.next();

				// If this matches, kill it.
				if(key.startsWith(keystart)) {
					i.remove();
				}
			} // for loop
		} // lock
	}

	////////////////////////////////////////////////////////////////////
	//                       Private Classes                          //
	////////////////////////////////////////////////////////////////////

	private class SpyCacheCleaner extends Thread {
		private TimeStampedHash cacheStore=null;
		private ReferenceQueue refQueue=null;

		// How many cleaning passes we've done.
		private int passes=0;

		// How many times were soft references dequeued.
		private int refDequeued=0;

		// This is so we can only report multicast security exceptions
		// once.
		private boolean reportedMulticastSE=false;

		// Insert multicast listener here.
		private CacheClearRequestListener listener=null;

		public SpyCacheCleaner(TimeStampedHash cacheStore,
			ReferenceQueue refQueue) {
			super();
			this.cacheStore=cacheStore;
			this.refQueue=refQueue;
			this.setName("SpyCacheCleaner");
			this.setDaemon(true);
			this.start();
		}

		public String toString() {
			return(super.toString() + " - "
				+ passes + " runs, mod age:  " + cacheStore.getUseAge()
				+ ", cur size:  " + cacheStore.size()
				+ ", tot stored:  " + cacheStore.getNumPuts()
				+ ", watermark:  " + cacheStore.getWatermark()
				+ ", hits:  " + cacheStore.getHits()
				+ ", misses:  " + cacheStore.getMisses()
				+ ", ref dqed:  " + refDequeued
				);
		}

		private void cleanup() throws Exception {
			long now=System.currentTimeMillis();
			synchronized(cacheStore) {
				for(Iterator i=cacheStore.keySet().iterator(); i.hasNext();){
					SpyCacheItem it=(SpyCacheItem)i.next();
					if(it.expired()) {
						i.remove();
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
				String addrS=System.getProperty("net.spy.cache.multi.addr");
				String portS=System.getProperty("net.spy.cache.multi.port");

				if(addrS!=null && portS!=null) {
					int port=Integer.parseInt(portS);

					InetAddress group = InetAddress.getByName(addrS);
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
					Reference r=refQueue.remove(300*1000);
					while(r!=null) {
						refDequeued++;
						r=refQueue.poll();
					}
					// Just for throttling, sleep a second.
					sleep(1000);
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
		private Reference value=null;
		private long exptime=0;

		public SpyCacheItem(Object key, Reference value, long cacheTime) {
			super();

			this.key=key;
			this.value=value;
			long t=System.currentTimeMillis();
			exptime=t+cacheTime;
		}

		public String toString() {
			String out="Cached item:  " + key;
			if(exptime>0) {
				out+="\nExpires:  " + new java.util.Date(exptime);
			}
			out+="\n";
			return(out);
		}

		public Reference getObject() {
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
			// If the reference is no longer valid, the object has expired
			if(value.get()==null) {
				ret=false;
			}
			return(ret);
		}
	} // SpyCacheItem

}
