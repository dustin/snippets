// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: URLWatcher.java,v 1.5 2002/08/21 05:22:48 dustin Exp $

package net.spy.net;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import java.net.URL;

import java.io.IOException;

import net.spy.cron.JobQueue;
import net.spy.cron.Cron;

/**
 * URLWatcher watches URLs and provides access to the most recent data from
 * the URL.
 */
public class URLWatcher extends Object {

	private static URLWatcher instance=null;

	private Cron cron=null;

	// How long to sleep between updates
	private static final int NAP_TIME=60000;

	private int numRuns=0;
	private boolean notdone=true;

	// This lets it know when to give up
	private int recentTouches=0;
	private int touchlessRuns=0;

	/**
	 * Get an instance of URLWatcher.
	 */
	private URLWatcher() {
		super();
		JobQueue jq=new JobQueue();
		cron=new Cron("URLWatcher Cron", jq);
	}

	/**
	 * Get the static instance of URLWatcher.
	 *
	 * @return the URLWatcher
	 */
	public static synchronized URLWatcher getInstance() {
		if(instance==null) {
			instance=new URLWatcher();
		}
		return(instance);
	}

	/**
	 * String me.
	 */
	public String toString() {
		int numPages=cron.getJobQueue().size();
		return(super.toString() + " - " + numPages + " pages monitored, "
			+ numRuns + " runs");
	}

	// Get the URLItem for the given URL.
	private URLItem getURLItem(URL u) {
		URLItem ui=null;

		JobQueue jq=cron.getJobQueue();
		synchronized(jq) {
			// Look at each item for the match
			for(Iterator i=jq.iterator(); ui==null && i.hasNext(); ) {
				URLItem tmp=(URLItem)i.next();

				if(tmp.getURL().equals(u)) {
					ui=tmp;
				} // It's a match
			} // All items
		} // lock

		return(ui);
	}

	/**
	 * Find out if this URLWatcher is watching a given URL.
	 *
	 * @param u the URL to test
	 * @return true if the URL is already being watched
	 */
	public boolean isWatching(URL u) {
		URLItem ui=getURLItem(u);
		return(ui != null);
	}

	/**
	 * Start watching the given URL.
	 * @param u The item to watch
	 */
	public void startWatching(URLItem u) {
		// Before adding it, make sure it's updated.
		u.runJob();
		JobQueue jq=cron.getJobQueue();
		synchronized(jq) {
			// Don't add it if it's already there
			if(!isWatching(u.getURL())) {
				jq.addJob(u);
			}
		}
	}

	/**
	 * Instruct the URLWatcher to stop URLWatching.
	 */
	public void shutdown() {
		synchronized(getClass()) {
			// Throw away the instance
			instance=null;
			// Shut down the cron
			cron.shutdown();
		}
	}

	/**
	 * Get the content (as a String) for a given URL.
	 *
	 * @param u The URL whose content we want
	 * @return The String contents, or null if none could be retreived
	 * @throws IOException if there was a problem updating the URL
	 */
	public String getContent(URL u) throws IOException {
		recentTouches++;
		URLItem ui=getURLItem(u);
		// If we don't have one for this URL yet, create it.
		if(ui==null) {
			ui=new URLItem(u);
			// Load the content
			ui.run();
			startWatching(ui);
		}
		// Return the current content.
		return(ui.getContent());
	}

}
