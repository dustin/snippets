// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: eBay.java,v 1.8 2002/07/10 05:41:33 dustin Exp $

package net.spy.info;

import java.util.Hashtable;

import net.spy.SpyUtil;

import net.spy.net.HTTPFetch;

/**
 * Get eBay info.
 */
public class eBay extends Info {

	/**
	 * Get an eBay object.
	 *
	 * @param itemNumber the item number to look up
	 */
	public eBay(String itemNumber) {
		super();
		this.arg = itemNumber;
	}

	/**
	 * Get an unitialized eBay object.
	 */
	public eBay() {
		super();
	}

	/**
	 * String me.
	 */
	public String toString() {
		String ret="";
		try {
			parseInfo();

			if(!error) {
				ret+=(String)hinfo.get("Description") + "\r\n"
					+ (String)hinfo.get("Currently") + "\r\n"
					+ (String)hinfo.get("High bid") + "\r\n"
					+ (String)hinfo.get("Time left");
			}
		} catch(Exception e) {
			// Who cares.
		}
		return(ret);
	}

	protected void parseInfo() throws Exception {
		if(hinfo==null) {
			hinfo=new Hashtable();
			hinfo.put("item_number", arg);
			getInfo();
            // System.out.println("Raw info:\n" + info);
			String lines[]=SpyUtil.split("\n", info);
			int section=0;
			for(int i=0; i<lines.length; i++) {
			    if(lines[i].startsWith("eBay item")) {
					i++;
					hinfo.put("Description", lines[i+4]);
					section++;
                } else if(lines[i].startsWith("Currently")) {
					i++;
					hinfo.put("Currently", lines[i]);
					section++;
				} else if(lines[i].startsWith("Time left")) {
					i++;
					hinfo.put("Time left", lines[i]);
					section++;
				} else if(lines[i].startsWith("High bid")) {
					i++;
					// Get rid of the score.
					int paren=lines[i].indexOf("(");
					String highbid=null;
					if(paren>0) {
						highbid=lines[i].substring(0, paren).trim();
					} else {
						highbid=lines[i];
					}
					hinfo.put("High bid", highbid);
					section++;
				}
			}
			if(section==0) {
				String errorString="Unable to get eBay info.  "
					+ "Invalid item number?";
				hinfo.put("ERROR", errorString);
			} else {
				error=false;
			}
		} // if there's a need to find it at all.
	}

	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://cgi.ebay.com/aw-cgi/eBayISAPI.dll?ViewItem&item=";
			url += arg;
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
		}
	}

	public static void main(String args[]) throws Exception {
		eBay e = new eBay(args[0]);
		System.out.println("Info:\n" + e);
		System.out.println("Info (XML):\n" + e.toXML());
	}
}
