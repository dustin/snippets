/*
 * Copyright (c) 2002  Dustin Sallings
 *
 * $Id: CacheTest.java,v 1.3 2002/08/13 04:18:10 dustin Exp $
 */

package net.spy.test;

import java.lang.ref.SoftReference;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import net.spy.cache.SpyCache;

/**
 * Test the cache system.
 */
public class CacheTest extends TestCase {

	// The ever-increasing value that will be stored in the cache.
	private int val=0;

    /**
     * Get an instance of CacheTest.
     */
    public CacheTest(String name) {
        super(name);
    }

	/**
	 * Get this test suite.
	 */
	public static Test suite() {
		return new TestSuite(CacheTest.class);
	}

	/**
	 * Run this test.
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	public void testBasicCaching() throws InterruptedException {
		SpyCache cache=SpyCache.getInstance();

		String key="testInt";

		Integer i=(Integer)cache.get(key);
		assertNull("Shouldn't be a value for " + key + " yet", i);

		// OK, now store it
		i=new Integer(++val);
		cache.store(key, i, 1000);

		// Check again immediately
		i=(Integer)cache.get(key);
		assertNotNull("Didn't get value for " + key, i);
		int tmp=i.intValue();
		assertEquals("Incorrect value returned from cache.", tmp, val);

		// Make sure enough time has passed
		Thread.sleep(1000);

		// Make sure we *don't* get the object from the cache
		i=(Integer)cache.get(key);
		assertNull(key + " should have expired by now", i);
	}

	public void testReferenceCaching() throws InterruptedException {
		SpyCache cache=SpyCache.getInstance();

		String key="testInt";

		Integer i=(Integer)cache.get(key);
		assertNull("Shouldn't be a value for " + key + " yet", i);

		// OK, now store it
		i=new Integer(++val);
		cache.store(key, new SoftReference(i), 1000);

		// Check again immediately
		i=(Integer)cache.get(key);
		assertNotNull("Didn't get value for " + key, i);
		int tmp=i.intValue();
		assertEquals("Incorrect value returned from cache.", tmp, val);

		// Make sure enough time has passed
		Thread.sleep(1000);

		// Make sure we *don't* get the object from the cache
		i=(Integer)cache.get(key);
		assertNull(key + " should have expired by now", i);
	}

}
