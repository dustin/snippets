// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FlightTracker.java,v 1.7 2002/07/10 05:41:27 dustin Exp $

package net.spy.info;

import java.util.Hashtable;
import java.util.StringTokenizer;

import net.spy.SpyUtil;

import net.spy.net.HTTPFetch;

/**
 * Get FlightTracker info.
 */
public class FlightTracker extends Info {

	private String airline=null;
	private String flightnum=null;

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
			String keys[]={"Airline", "Number", null, "Origination",
							"OriginationTime", "Status", "Location",
							"Altitude", "Speed", "Equipment", null,
							"Destination", "ArrivalTime",
							"DestinationDistance"};
			int section=0;
			int keyCounter=0;
			String localInfo = "";
			for(int i=0; i<lines.length; i++) {
				if(lines[i].startsWith("AirlineFlight")) {
					i++;
					section=1;
					error=false;
				} else if(lines[i].startsWith("Having a problem")
					&& section==1) {
					section=2;
				}

				// We've figured out what section we're in, now let's look
				// at the data.
				if(section==1) {
					localInfo+=lines[i] + "\r\n";

					if (keys.length >= keyCounter && keys[keyCounter] != null) {
						hinfo.put(keys[keyCounter], lines[i]);
					}
					keyCounter++;


				}
			}
			if(error) {
				String errorString="Unable to get FlightTracker info.  "
					+ "Invalid flight info?";
				hinfo.put("ERROR", errorString);
			} else {
				localInfo=localInfo.trim();
				hinfo.put("info", localInfo);
			}
		} // if there's a need to find it at all.
	}

	protected void getInfo() throws Exception {
		if(info==null) {
			String url=
				"http://www.trip.com/ft/results/1,2093,1-1,00.shtml?Airline="
					+ airline + "&FlightNumber=" + flightnum
					+ "&Command=ByFlightNumber";
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
