// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>

package net.spy.net;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import net.spy.SpyUtil;

/**
 * Oversimplified HTTP document fetcher.
 */

// Fetch the contents of a URL
public class HTTPFetch {
	private URL url;

	private String contents=null;
	private String stripped=null;

	private Hashtable headers=null;

	/**
	 * Create a new HTTPFetch object for a given string representation of a
	 * URL.
	 *
	 * @param u String representation of the URL we'll be connecting to.
	 *
	 * @exception MalformedURLException Thrown if the URL cannot be parsed.
	 */
	public HTTPFetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	/**
	 * Create a new HTTPFetch object for a given string representation of a
	 * URL, including a hash describing extra headers to add.
	 *
	 * @param u String representation of the URL we'll be connecting to.
	 * @param head Hashtable containing headers to set.
	 *
	 * @exception MalformedURLException Thrown if the URL cannot be parsed.
	 */
	public HTTPFetch(String u, Hashtable head) throws MalformedURLException {
		super();
		url=new URL(u);
		headers=head;
	}

	/**
	 * Get a vector containing the individual lines of the document
	 * returned from the URL.
	 *
	 * @exception Exception thrown when something fails.
	 */
	public Vector getLines() throws Exception {
		Vector v = new Vector();
		getData();

		String lines[]=SpyUtil.split("\r\n", contents);
		for(int i=0; i<lines.length; i++) {
			v.addElement(lines[i]);
		}

		return(v);
	}

	/**
	 * Return the contents of the URL as a whole string.
	 *
	 * @exception Exception thrown when something fails.
	 */
	public String getData() throws Exception {
		if(contents==null) {
			contents="";
			StringBuffer sb=new StringBuffer();
			BufferedReader br = getReader();
			String line;
			while( (line=br.readLine()) != null) {
				sb.append(line);
				sb.append("\n");
			}
			contents=sb.toString();
		}
		return(contents);
	}

	/**
	 * Return the contents of the URL with the HTML tags stripped out.
	 *
	 * @exception Exception thrown when something fails.
	 */
	public String getStrippedData() throws Exception {
		getData();
		if(stripped==null) {
			int inTag=0;
			StringBuffer sb=new StringBuffer();

			char chars[]=contents.toCharArray();

			for(int i=0; i<chars.length; i++) {
				if(chars[i]=='<') {
					inTag++;
				} else if( chars[i]=='>' && inTag>0) {
					if(inTag>=1) {
						inTag--;
					}
				} else {
					if(inTag==0) {
						sb.append(chars[i]);
					}
				}
			}
			stripped=sb.toString();
		}
		return(stripped);
	}

	// Get a reader for the above routines.
	private BufferedReader getReader() throws Exception {
		URLConnection uc = url.openConnection();
		if(headers!=null) {
			for(Enumeration e=headers.keys(); e.hasMoreElements(); ) {
				String key=(String)e.nextElement();
				String value=(String)headers.get(key);

				uc.setRequestProperty(key, value);
			}
		}
		InputStream i = uc.getInputStream();
		BufferedReader br =
			new BufferedReader( new InputStreamReader(i));
		return(br);
	}

	public static void main(String args[]) throws Exception {
		HTTPFetch hf=new HTTPFetch(args[0]);
		System.out.println(hf.getStrippedData());
	}
}
