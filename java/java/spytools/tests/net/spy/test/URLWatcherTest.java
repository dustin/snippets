// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: URLWatcherTest.java,v 1.1 2002/08/20 08:04:41 dustin Exp $

package net.spy.test;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import java.io.IOException;
import java.net.URL;

import net.spy.net.URLWatcher;
import net.spy.net.URLItem;

import net.spy.cron.SimpleTimeIncrement;

/**
 * Test the URLWatcher thing.
 */
public class URLWatcherTest extends TestCase {

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
	 * Test basic URL watching functionality.
	 */
	public void testBasicURLWatching()
		throws IOException, InterruptedException {

		URLWatcher uw=URLWatcher.getInstance();
		URL u=new URL("http://bleu.west.spy.net/~dustin/util/getdate.jsp");
		String c1=uw.getContent(u);
		assertNotNull(c1);
		Thread.sleep(1000);
		String c2=uw.getContent(u);
		assertNotNull(c2);

		assertEquals(c1 + "!=" + c2, c1, c2);

		uw.shutdown();
	}

	/**
	 * Test a URLWatcher with a manual URL setting.
	 */
	public void testManualURLWatching()
		throws IOException, InterruptedException {

		URLWatcher uw=URLWatcher.getInstance();
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

}
