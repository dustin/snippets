// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>

package net.spy.net;

import java.net.*;
import java.io.*;
import java.util.*;

/**
 * Oversimplified HTTP document fetcher.
 */

// Fetch the contents of a URL
public class HTTPFetch {
	URL url;

	/**
	 * Create a new HTTPFetch object for a given string representation of a
	 * URL.
	 *
	 * @exception MalformedURLException Thrown if the URL cannot be parsed.
	 */
	public HTTPFetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	/**
	 * Get a vector containing the individual lines of the document
	 * returned from the URL.
	 *
	 * @exception Exception Any failures keeping the data from being
	 * retrieved will be sent from here.
	 */
	public Vector getLines() throws Exception {
		Vector v = new Vector();

		try {
			BufferedReader br = getReader();
			String line;
			while( (line=br.readLine()) != null) {
				v.addElement(line);
			}
		} catch(Exception e) {
			throw new Exception(e.getMessage());
		}

		return(v);
	}

	/**
	 * Return the contents of the URL as a whole string.
	 *
	 * @exception Exception Any failures keeping the data from being
	 * retrieved will be sent from here.
	 */
	public String getData() throws Exception {
		String s = "";

		try {
			BufferedReader br = getReader();
			String line;
			while( (line=br.readLine()) != null) {
				s+=line + "\n";
			}
		} catch(Exception e) {
			throw new Exception(e.getMessage());
		}
		return(s);
	}

	// Get a reader for the above two routines.
	protected BufferedReader getReader() throws Exception {
		URLConnection uc = url.openConnection();
		InputStream i = uc.getInputStream();
		BufferedReader br =
			new BufferedReader( new InputStreamReader(i));
		return(br);
	}
}
