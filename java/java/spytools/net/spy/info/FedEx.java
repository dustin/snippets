// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FedEx.java,v 1.7 2001/03/03 11:25:59 dustin Exp $

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

	private void setValue(Hashtable h, String input, String match) {
		if(input.startsWith(match)) {
			// Make sure it's likely we'd be able to get it and trim it
			if(input.length() > match.length()+1) {
				String data=input.substring(match.length()+1).trim();
				// No dta, no addition
				if(data.length()>0) {
					h.put(match, data);
				}
			}
		}
	}

	protected void parseInfo() throws Exception {
		hinfo=new Hashtable();
		hinfo.put("tracking_number", arg);
		getInfo();
		String lines[]=SpyUtil.split("\n", info);
		for(int i=0; i<lines.length; i++) {
			setValue(hinfo, lines[i], "Tracking Number");
			setValue(hinfo, lines[i], "Shipper ID");
			setValue(hinfo, lines[i], "Customer Reference Number");
			setValue(hinfo, lines[i], "Invoice Number");
			setValue(hinfo, lines[i], "Purchase Order Number");
			setValue(hinfo, lines[i], "Ship Date");
			setValue(hinfo, lines[i], "E-PDI Date");
			setValue(hinfo, lines[i], "Est. Delivery Date");
			setValue(hinfo, lines[i], "Status");
			setValue(hinfo, lines[i], "Delivery Location");
			setValue(hinfo, lines[i], "Delivery Date/Time");
			setValue(hinfo, lines[i], "Signed For By");
			setValue(hinfo, lines[i], "Service Type");
			setValue(hinfo, lines[i], "Total Weight");
		}
		// If there's no tracking number in the hash, it wasn't succesful
		if(hinfo.get("Tracking Number")!=null) {
			error=false;
		}
		// If there's a delivery date and time, it's been delivered
		if(hinfo.get("Delivery Date/Time")!=null) {
			delivered=true;
		}
		if(error) {
			String error_string="Unable to get FedEx info.  "
				+ "Invalid tracking number?";
			hinfo.put("ERROR", error_string);
		} else {
			// Make a toStringable version
			StringBuffer sb=new StringBuffer();
			sb.append("Tracking ");
			sb.append(hinfo.get("Tracking Number"));
			if(delivered) {
				sb.append(" - delivered.");
			} else {
				if(hinfo.get("Est. Delivery Date")!=null) {
					sb.append(" - ETA: ");
					sb.append(hinfo.get("Est. Delivery Date"));
				}
				if(hinfo.get("Status")!=null) {
					sb.append(" - Status: ");
					sb.append(hinfo.get("Status"));
				}
			}
			hinfo.put("info", sb.toString().trim());
		}
	}

	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://www.fedex.com/cgi-bin/tracking?tracknumbers=";
			url += arg;
			url += "&action=track&language=english&cntry_code=us";
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
