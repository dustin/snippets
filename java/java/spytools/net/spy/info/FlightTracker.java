// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FlightTracker.java,v 1.3 2000/06/27 19:17:17 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Get FlightTracker info.
 */

public class FlightTracker extends Info {

	String airline=null;
	String flightnum=null;

	/**
	 * Get a FlightTracker object.
	 *
	 * @param airline the 3-letter airline abreviation
	 * @param flightnum the flight number on that airline
	 */
	public FlightTracker(String airline, String flightnum) {
		super();
		this.airline = airline;
		this.flightnum = flightnum;
	}

	/**
	 * Get a FlightTracker object.
	 *
	 * @param arg airline and flight number with a colon separating them.
	 * i.e. DFW:3832
	 */
	public FlightTracker(String arg) {
		super();
		setArg(arg);
	}

	/**
	 * Get an uninitialized FlightTracker object.
	 */
	public FlightTracker() {
		super();
	}

	/**
	 * Set the object arguments.
	 *
	 * @param arg airline and flight number with a colon separating them.
	 * i.e. DFW:3832
	 */
	public void setArg(String arg) {
		this.arg=arg;
		StringTokenizer st = new StringTokenizer(arg, ": ");
		airline=st.nextToken();
		flightnum=st.nextToken();
	}

	public String toString() {
		String ret="";
		try {
			parseInfo();

			if(! error) {
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
			hinfo.put("airline", airline);
			getInfo();
			String lines[]=SpyUtil.split("\n", info);
			String keys[]={"Airline", "Number", null, "Origination", "OriginationTime", "Status",
							"Location", "Altitude", "Speed", "Equipment", null, "Destination",
							"ArrivalTime", "DestinationDistance"};
			int section=0;
			int keyCounter=0;
			String local_info = "";
			for(int i=0; i<lines.length; i++) {
				if(lines[i].startsWith("AirlineFlight")) {
					i++;
					section=1;
					error=false;
				} else if(lines[i].startsWith("Having a problem") && section==1) {
					section=2;
				}

				// We've figured out what section we're in, now let's look
				// at the data.
				if(section==1) {
					local_info+=lines[i] + "\r\n";

					if (keys.length >= keyCounter && keys[keyCounter] != null) {
						hinfo.put(keys[keyCounter], lines[i]);
					}
					keyCounter++;


				}
			}
			if(error) {
				String error_string="Unable to get FlightTracker info.  "
					+ "Invalid flight info?";
				hinfo.put("ERROR", error_string);
			} else {
				local_info=local_info.trim();
				hinfo.put("info", local_info);
			}
		} // if there's a need to find it at all.
	}

	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://www.trip.com/ft/results/1,2093,1-1,00.shtml?Airline=" + airline
					+ "&FlightNumber=" + flightnum + "&Command=ByFlightNumber";
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
		}
	}

	public static void main(String args[]) throws Exception {
		FlightTracker f =null;

		if(args.length > 1) {
			f=new FlightTracker(args[0], args[1]);
		} else {
			f=new FlightTracker();
			f.setArg(args[0]);
		}
		System.out.println("Info:\n" + f);
		System.out.println("Info (XML):\n" + f.toXML());
	}
}
