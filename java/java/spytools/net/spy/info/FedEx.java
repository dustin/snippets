// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FedEx.java,v 1.1 2000/05/01 07:05:43 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Get FedEx info.
 */

public class FedEx extends Info {

	String airbill_number=null;

	/**
	 * Get an FedEx object.
	 *
	 * @param airbill_number the item number to look up
	 */
	public FedEx(String airbill_number) {
		super();
		this.airbill_number = airbill_number;
	}

	public String toString() {
		String ret="";
		try {
			if(error) {
				ret=get("info");
			} else {
				ret=get("ERROR");
			}
		} catch(Exception e) {
			// Who cares.
		}
		return(ret);
	}

	protected void parseInfo() throws Exception {
		if(hinfo==null) {
			hinfo=new Hashtable();
			hinfo.put("airbill_number", airbill_number);
			getInfo();
			String lines[]=SpyUtil.split("\n", info);
			int section=0;
			String local_info = "";
			for(int i=0; i<lines.length; i++) {
				if(lines[i].startsWith("Airbill Found")) {
					section=1;
					error=false;
				} else if(lines[i].startsWith(airbill_number) && section==1) {
					section=2;
				} else if(lines[i].startsWith("If you have any questions")
					&& section==2) {
					section=3;
				}

				// We've figured out what section we're in, now let's look
				// at the data.
				if(section==2) {
					System.out.println("Adding:  " + lines[i]);
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
		} // if there's a need to find it at all.
	}

	// 790827254891 - Tracking this...
	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://www.fedex.com/cgi-bin/track_it?airbills=";
			url += airbill_number;
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
		}
	}

	public static void main(String args[]) throws Exception {
		FedEx f = new FedEx(args[0]);
		System.out.println("Info:\n" + f);
		System.out.println("Info (XML):\n" + f.toXML());
	}
}
