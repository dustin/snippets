// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: URLWatcherTest.java,v 1.6 2002/08/24 07:23:04 dustin Exp $

package net.spy.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;

import net.spy.net.URLWatcher;
import net.spy.net.URLItem;

import net.spy.cron.SimpleTimeIncrement;

/**
 * Test the URLWatcher thing.
 */
public class URLWatcherTest extends TestCase {

	private URLWatcher uw=null;

	/**
	 * Get an instance of URLWatcherTest.
	 */
	public URLWatcherTest(String name) {
		super(name);
	}

	/**
	 * Get this test suite.
	 */
	public static Test suite() {
		return(new TestSuite(URLWatcherTest.class));
	}

	/**
	 * Run this test.
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	/**
	 * Get the URLWatcher.
	 */
	protected void setUp() {
		uw=URLWatcher.getInstance();
	}

	/**
	 * Get rid of the URLWatcher.
	 */
	protected void tearDown() {
		uw.shutdown();
	}

	/**
	 * Test basic URL watching functionality.
	 */
	public void testBasicURLWatching()
		throws IOException, InterruptedException {

		URL u=new URL("http://bleu.west.spy.net/~dustin/util/getdate.jsp");
		String c1=uw.getContent(u);
		assertNotNull(c1);
		Thread.sleep(1000);
		String c2=uw.getContent(u);
		assertNotNull(c2);

		assertSame("Different results", c1, c2);
		assertEquals(c1 + "!=" + c2, c1, c2);
	}

	/**
	 * Test a URLWatcher with a manual URL setting.
	 */
	public void testManualURLWatching()
		throws IOException, InterruptedException {

		URL u=new URL("http://bleu.west.spy.net/~dustin/util/getdate.jsp");

		if(uw.isWatching(u)) {
			System.err.println(uw.getContent(u));
		}

		assertTrue("Shouldn't be watching that URL yet.", (!uw.isWatching(u)));

		URLItem ui=new URLItem(u, new SimpleTimeIncrement(3000));
		uw.startWatching(ui);

		String s1=uw.getContent(u);
		assertNotNull("Initial content not returned", s1);
		String s2=uw.getContent(u);
		assertNotNull("Second content not returned", s1);

		assertSame("Different results second time", s1, s2);

		Thread.sleep(5000);

		String s3=uw.getContent(u);
		assertNotNull("Third content not returned", s3);
		assertTrue("Expected different results on third run", (!s2.equals(s3)));
	}

	/**
	 * Test with several URLs.
	 */
	public void testLotsOfURLs() throws IOException, InterruptedException {
		String urls[]={"http://bleu.west.spy.net/",
			"http://bleu.west.spy.net/~dustin/",
			"http://bleu.west.spy.net/~dustin/music/",
			"http://bleu.west.spy.net/~dustin/eiffel/",
			"http://bleu.west.spy.net/~dustin/projects/",
			"http://bleu.west.spy.net/~dustin/wa/bleu/",
			"http://bleu.west.spy.net/~dustin/wa/prop/",
			"http://bleu.west.spy.net/~dustin/projects/filemonitor.xtp",
			"http://bleu.west.spy.net/~dustin/projects/spytest.xtp",
			"http://bleu.west.spy.net/~dustin/projects/spyjar.xtp"};

		HashMap content=new HashMap();

		for(int i=0; i<urls.length; i++) {
			URL u=new URL(urls[i]);
			String s=uw.getContent(u);
			assertNotNull("Didn't get content for " + u, s);
			content.put(u, s);
			// Sleep a bit after we get going.
			if(i>5) {
				Thread.sleep(500);
			}
		}

		// Wait two seconds
		Thread.sleep(2000);

		for(int i=0; i<urls.length; i++) {
			URL u=new URL(urls[i]);
			assertTrue("Not watching " + u, uw.isWatching(u));

			String s=uw.getContent(u);
			assertNotNull("Didn't get content for " + u + " 2nd time", s);

			// Verify it looks the same.
			String s1=(String)content.get(u);
			assertNotNull("Saved content was null for " + u, s1);

			assertEquals("Second run was different for " + u, s1, s);
			assertSame("Second run was a different instance for " + u, s1, s);
		}
	}
}
