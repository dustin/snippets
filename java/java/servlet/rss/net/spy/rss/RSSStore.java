// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSStore.java,v 1.2 2001/04/26 21:02:14 dustin Exp $

package net.spy.rss;

import java.util.*;
import net.spy.net.*;

/**
 * Take care of the relationships with all the RSS vendors.
 */
public class RSSStore extends Thread {

	// Don't let them idle more than an hour
	private static final int MAX_IDLE_TIME=3600000;

	private Hashtable sites=null;
	private boolean notdone=true;
	private int runs=0;

	/**
	 * Get an instance of RSSStore.
	 */
	public RSSStore() {
		super("RSSStore");
		setDaemon(true);
		sites=new Hashtable();
	}

	/**
	 * Get a String telling about this thing.
	 */
	public String toString() {
		int numSites=sites.size();
		return(super.toString() + " - "
			+ numSites + " sites watched, " + runs + " runs");
	}

	/**
	 * Update all the thingies.
	 */
	public void run() {
		// Flip through the list every five minutes and get it updated.
		while(notdone) {
			runs++;
			long now=System.currentTimeMillis();
			Vector toremove=new Vector();
			// Update and queue removals
			for(Enumeration e=sites.elements(); e.hasMoreElements(); ) {

				RSSItem ri=(RSSItem)e.nextElement();
				// Throw away anything that's too old, otherwise, update it
				if( (now-ri.lastRequest()) > MAX_IDLE_TIME) {
					toremove.addElement(ri.getURL());
				} else {
					ri.update();
				}
			}
			// Remove the things that we don't want anymore.
			for(Enumeration e=toremove.elements(); e.hasMoreElements(); ) {
				String key=(String)e.nextElement();
				sites.remove(key);
			}
			// Try to take a nap.
			try {
				sleep(5*60*1000);
			} catch(Exception e) {
				// Don't worry about this one.
			}
		}
		System.err.println("RSS Thread shutting down.");
	}

	/**
	 * Prepare for a shutdown.
	 */
	public void shutdown() {
		// Double negative!
		notdone=false;
	}

	/**
	 * Get the content for a URL.
	 */
	public String getContent(String url) {
		RSSItem ri=(RSSItem)sites.get(url);
		// If we don't have one for this URL yet, get one.
		if(ri==null) {
			// Lock the sites hash while we create this new one.
			synchronized(sites) {
				ri=new RSSItem(url);
				sites.put(url, ri);
			}
			// Tell it to get its shit.
			ri.update();
		}
		return(ri.getContent());
	}

}
