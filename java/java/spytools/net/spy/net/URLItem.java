// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: URLItem.java,v 1.1 2002/08/19 07:08:31 dustin Exp $

package net.spy.net;

import java.util.HashMap;

import java.io.IOException;

import java.net.URL;

/**
 * A particular URL that's being watched.
 */
public class URLItem extends Object {

	// How often to update.
	private int erval=1800;
	// How long a URL will be watched if nobody wants it (defaults to an
	// hour).
	private int maxIdleTime=3600000;

	// Last update
	private long lastUpdate=0;
	private long lastRequest=0;

	private URL url=null;

	private String content=null;

	private IOException lastError=null;

	/**
	 * Get an instance of URLItem.
	 */
	public URLItem(URL u) {
		super();
		this.url=u;
	}

	/**
	 * Ask the URL to update itself if it needs to.
	 */
	public void update() {
		long now=System.currentTimeMillis();
		int timepassed=(int)((now-lastUpdate)/1000);
		HashMap headers=new HashMap();
		// make sure the stuff isn't cached
		headers.put("Pragma", "no-cache");

		// Don't update unless we need to
		if(timepassed > erval) {
			try {
				HTTPFetch hf=new HTTPFetch(url, headers);
				content=hf.getData();
				lastUpdate=now;
			} catch(IOException e) {
				lastError=e;
			}
		}
	}

	/**
	 * Get the content from the last fetch.
	 */
	public String getContent() throws IOException {
		lastRequest=System.currentTimeMillis();
		if(lastError!=null) {
			throw lastError;
		}
		return(content);
	}

	/**
	 * Find out when the last request was.
	 *
	 * @return the timestamp of the last request.
	 */
	public long getLastRequest() {
		return(lastRequest);
	}

	/**
	 * Get the URL this thing is watching.
	 *
	 * @return
	 */
	public URL getURL() {
		return(url);
	}

	/**
	 * Set the update frequency.
	 *
	 * @param to the number of seconds between updates
	 */
	public void setInterval(int to) {
		erval=to;
	}

	/**
	 * Set the maximum number of milliseconds this URL will remain in the
	 * container if nothing requests it.
	 */
	public void setMaxIdleTime(int maxIdleTime) {
		this.maxIdleTime=maxIdleTime;
	}

	/**
	 * Get the maximum number of milliseconds this URL will remain in the
	 * container if nothing requests it.
	 */
	public int getMaxIdleTime() {
		return(maxIdleTime);
	}

}
