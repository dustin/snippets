// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FedEx.java,v 1.4 2000/06/16 20:42:28 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Get FedEx info.
 */

public class FedEx extends PackageInfo {

	/**
	 * Get an FedEx object.
	 *
	 * @param airbill_number the item number to look up
	 */
	public FedEx(String airbill_number) {
		super();
		this.arg = airbill_number;
	}

	/**
	 * Instantiate a FedEx object with no argument
	 */
	public FedEx() {
		super();
	}

	public String toString() {
		String ret="";
		try {
			// Make sure we got something...
			get("ERROR");
			if(error) {
				ret=get("ERROR");
			} else {
				ret=get("info");
			}
			// Let's see if we got a delivered status
			if(ret.indexOf("Delivered")>=0) {
				delivered=true;
			}
		} catch(Exception e) {
			// Who cares.
			System.err.println("Exception on FedEx.toString():  " + e);
		}
		return(ret);
	}

	protected void parseInfo() throws Exception {
		hinfo=new Hashtable();
		hinfo.put("airbill_number", arg);
		getInfo();
		String lines[]=SpyUtil.split("\n", info);
		int section=0;
		String local_info = "";
		for(int i=0; i<lines.length; i++) {
			if(lines[i].startsWith("Airbill Found")) {
				section=1;
				error=false;
			} else if(lines[i].startsWith(arg) && section==1) {
				section=2;
			} else if(lines[i].startsWith("If you have any questions")
				&& section==2) {
				section=3;
			}

			// We've figured out what section we're in, now let's look
			// at the data.
			if(section==2) {
				local_info+=lines[i] + "\r\n";
			}
		}
		if(error) {
			String error_string="Unable to get FedEx info.  "
				+ "Invalid tracking number?";
			hinfo.put("ERROR", error_string);
		} else {
			local_info=local_info.trim();
			hinfo.put("info", local_info);
		}
	}

	// 790827254891 - Tracking this...
	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://www.fedex.com/cgi-bin/track_it?airbills=";
			url += arg;
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
		}
	}

	public static void main(String args[]) throws Exception {
		FedEx f = new FedEx(args[0]);
		System.out.println("Info:\n" + f);
		System.out.println("Delivery status:  " + f.isDelivered());
		System.out.println("Info (XML):\n" + f.toXML());
	}
}
