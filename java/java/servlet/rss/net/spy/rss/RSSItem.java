// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RSSItem.java,v 1.1 2001/04/26 19:16:00 dustin Exp $

package net.spy.rss;

import net.spy.net.*;

/**
 * An individual RSS item from a site.
 */
public class RSSItem extends Object {

	// How often to update (in seconds)
	private int erval=1800;
	// Last update
	private long lastUpdate=0;

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

		// Only update if we need to
		if(timepassed>erval) {
			try {
				HTTPFetch hf=new HTTPFetch(url);
				xml=hf.getData();
				lastUpdate=now;
			} catch(Exception e) {
				lastError=e;
			}
		}
	}

	/**
	 * Get the content from the RSS.
	 */
	public String getContent() {
		if(xml==null) {
			xml="<no_data_found/>";
		}
		return(xml);
	}

	/**
	 * Set the update frequency.  Anything less than five minutes really
	 * doesn't matter.
	 */
	public void setInterval(int to) {
		erval=to;
	}
}
