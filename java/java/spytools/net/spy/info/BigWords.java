// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: BigWords.java,v 1.1 2000/06/20 07:15:09 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Look up a book by ISBN from BigWords.com.
 */

public class BigWords extends Info {

	/**
	 * Get a BigWords object.
	 *
	 * @param isbn The ISBN of the book we want to look at.
	 */
	public BigWords(String isbn) {
		super();
		this.arg = isbn;
	}

	/**
	 * Get an unitialized Traffic object.
	 */
	public BigWords() {
		super();
	}

	public String toString() {
		String ret="";
		try {
			get("ERROR"); // get the juices flowing
			if(error) {
				ret=get("ERROR");
			} else {
				ret=get("title") + " by " + get("author")
					+ " - Format: " + get("format") + " - "
					+ "Used: " + get("used_price")
						+ " (" + get("used_availability") + "), "
					+ "New: " + get("new_price")
						+ " (" + get("new_availability") + ")";
			}
		} catch(Exception e) {
			System.err.println("Exception on BigWords.toString():  " + e);
			e.printStackTrace();
		}
		return(ret);
	}

	protected void parseInfo() throws Exception {
		hinfo=new Hashtable();
		hinfo.put("isbn", arg);
		getInfo();
		String lines[]=SpyUtil.split("\n", info);
		int section=0;
		String local_info = "";
		for(int i=0; i<lines.length; i++) {
			if(lines[i].startsWith("BOOK ZOOM")) {
				i++;
				hinfo.put("title", lines[i]);
				i++;
				hinfo.put("author", lines[i].substring(3));
			} else if(lines[i].startsWith("NEW:")) {
				i++;
				hinfo.put("new_price", lines[i]);
				i++;
				hinfo.put("new_availability", lines[i].substring(11));
			} else if(lines[i].startsWith("USED:")) {
				i++;
				hinfo.put("used_price", lines[i]);
				i++;
				hinfo.put("used_availability", lines[i].substring(11));
			} else if(lines[i].startsWith("FORMAT")) {
				String stuff[]=SpyUtil.split(":", lines[i]);
				hinfo.put("format", stuff[1].trim());
			} else if(lines[i].startsWith("PUBLISHER")) {
				String stuff[]=SpyUtil.split(":", lines[i]);
				hinfo.put("publisher", stuff[1].trim());
			} else if(lines[i].startsWith("PUBLISHED")) {
				String stuff[]=SpyUtil.split(":", lines[i]);
				hinfo.put("publish_date", stuff[1].trim());
			}
		}
		if(get("publish_date")==null) {
			error=true;
		} else {
			error=false;
		}
		if(error) {
			String error_string="Unable to get Book info.  "
				+ "Invalid or unknown ISBN?";
			hinfo.put("ERROR", error_string);
		} else {
			local_info=local_info.trim();
			hinfo.put("info", local_info);
		}
	} // if there's a need to find it at all.


	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://bigwords.com/search/index.cfm?"
					+ "BIGVERB=book%20zoom&ISBN="
					+ arg;
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
		}
	}

	public static void main(String args[]) throws Exception {
		BigWords b = new BigWords(args[0]);
		System.out.println("Info:\n" + b);
		System.out.println("Info (XML):\n" + b.toXML());
	}
}
