// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSItem.java,v 1.5 2002/08/16 07:28:42 dustin Exp $

package net.spy.rss;

import java.util.HashMap;
import net.spy.net.*;

/**
 * An individual RSS item from a site.
 */
public class RSSItem extends Object {

	// How often to update (in seconds)
	private int erval=1800;
	// Last update
	private long lastUpdate=0;
	private long lastRequest=0;

	private String url=null;

	private String xml=null;

	private Exception lastError=null;

	/**
	 * Get an instance of RSSItem.
	 */
	public RSSItem(String u) {
		super();
		this.url=u;
	}

	/**
	 * Request the RSS to update itself if it needs to.
	 */
	public void update() {
		long now=System.currentTimeMillis();
		int timepassed=(int)((now-lastUpdate)/1000);
		HashMap headers=new HashMap();
		// Make sure we get good shit.
		headers.put("Pragma", "no-cache");

		// Only update if we need to
		if(timepassed>erval) {
			try {
				HTTPFetch hf=new HTTPFetch(url, headers);
				xml=hf.getData();
				lastUpdate=now;
				System.err.println("RSSServlet updated " + url + " at "
					+ new java.util.Date(lastUpdate));
			} catch(Exception e) {
				lastError=e;
			}
		}
	}

	/**
	 * Get the content from the RSS.
	 */
	public String getContent() {
		lastRequest=System.currentTimeMillis();
		if(xml==null) {
			xml="<no_data_found/>";
		}
		return(xml);
	}

	/**
	 * Find out the last time someone cared about this news item.
	 */
	public long lastRequest() {
		return(lastRequest);
	}

	/**
	 * Find out what this thing is looking at.
	 */
	public String getURL() {
		return(url);
	}

	/**
	 * Set the update frequency.  Anything less than five minutes really
	 * doesn't matter.
	 */
	public void setInterval(int to) {
		erval=to;
	}
}
