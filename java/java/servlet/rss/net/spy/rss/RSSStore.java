// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSStore.java,v 1.1 2001/04/26 19:16:02 dustin Exp $

package net.spy.rss;

import java.util.*;
import net.spy.net.*;

/**
 * Take care of the relationships with all the RSS vendors.
 */
public class RSSStore extends Thread {

	private Hashtable sites=null;
	private boolean notdone=true;

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
		return(super.toString() + " - " + numSites + " sites watched.");
	}

	/**
	 * Update all the thingies.
	 */
	public void run() {
		// Flip through the list every five minutes and get it updated.
		while(notdone) {
			for(Enumeration e=sites.elements(); e.hasMoreElements(); ) {
				RSSItem ri=(RSSItem)e.nextElement();
				ri.update();
			}
			try {
				sleep(5*60*1000);
			} catch(Exception e) {
				// Don't worry about this one.
			}
		}
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
