// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: UPS.java,v 1.11 2002/07/10 04:25:39 dustin Exp $

package net.spy.info;

import java.util.Hashtable;

import net.spy.SpyUtil;

import net.spy.net.HTTPFetch;

/**
 * Get UPS info.
 *
 * <p>
 * The following variables are available as of this writing:
 * <p>
 * <ul>
 *  <li>Package Status - UPS' idea of the state of the delivery.</li>
 *  <li>Last Scanned on - Last time the package was scanned.</li>
 *  <li>Last Scanned at - Last location where the package was scanned.</li>
 *  <li>Sent on - Date the package was sent.</li>
 *  <li>Shipped to - Destination of the package.</li>
 *  <li>Tracking Number - UPS tracking number of the package.</li>
 *  <li>UPS Service Type - Um, it's not obvious?</li>
 *  <li>Package Weight - Approximate weight of the package.</li>
 *  <li>NOTICE - Legal shite.</li>
 * </ul>
 */

public class UPS extends PackageInfo {

	/**
	 * Get a UPS info object.
	 *
	 * @param tracking_number tracking number to look up
	 */
	public UPS(String tracking_number) {
		super();
		this.arg = tracking_number;
	}

	/**
	 * Get an unitialized UPS object.
	 */
	public UPS() {
		super();
	}

	/**
	 * Get a string representation of the UPS object.
	 */
	public String toString() {
		String ret="";
		try {
			// Force it to have done everything it needs.
			get("ERROR");
			// Deal with not getting our data.
			if(error) {
				ret+=get("ERROR", "An unknown error has occurred");
			} else {
				ret+="Status:  " + get("Package Status", "unknown") + ", ";
				ret+="Location:  " + get("Last Scanned at", "unknown") + ", ";
				ret+="When:  " + get("Last Scanned on", "unknown");
			}
			// Check to see if the thing's been delivered.
			String delcheck=get("Package Status");
			if(delcheck!=null) {
				if(delcheck.startsWith("Delivered")) {
					delivered=true;
				}
			}
		} catch(Exception e) {
			// Just let it return null
			System.err.println("WTF!: " + e);
			e.printStackTrace();
		}
		return(ret);
	}

	protected void parseInfo() throws Exception {
		if(hinfo==null) {
			hinfo=new Hashtable();
			getInfo();
			String lines[]=SpyUtil.split("\n", info);
			int section=0;
			for(int i=0; i<lines.length; i++) {
				// we really only care about the first section
				if(lines[i].startsWith("Tracking Result")) {
					section=1;
				} else if(lines[i].startsWith("NOTICE:")) {
					section=2;
				}

				// Figure out what section we're parsing.
				if(section==1) {
					int sep=lines[i].indexOf(":&nbsp;");
					if(sep>0) {
						String k, v;
						k=deEntity(lines[i].substring(0, sep));
						v=deEntity(lines[i].substring(sep+7,
							lines[i].length()));
						hinfo.put(k, v);
					} // Sep index
				} // Section one
			} // For loop through lines
			if(info.indexOf("Unable to track shipment") >= 0) {
				hinfo.put("ERROR", "Unable to track shipment.  "
					+ "Invalid tracking number?");
			} else {
				error=false;
			}
		} // if there's a need to find it at all.
	}

	protected void getInfo() throws Exception {
		if(info==null) {
			String url="http://wwwapps.ups.com/tracking/tracking.cgi?tracknum=";
			url += arg;
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
		}
	}

	public static void main(String args[]) throws Exception {
		UPS u = new UPS(args[0]);
		System.out.println("Info:\n" + u);
		System.out.println("Delivery status:  " + u.isDelivered());
		System.out.println("Info (XML):\n" + u.toXML());
	}
}
