// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Weather.java,v 1.10 2001/06/21 00:07:31 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Get Weather info from weather.com
 */

public class Weather extends Info {

	private String relevent=null;
	private String shortWeather=null;
	private String city=null;

	/**
	 * Get a Weather object.
	 *
	 * @param zip_code zip code to look up weather for.
	 */
	public Weather(String zip_code) {
		super();
		this.arg = zip_code;
	}

	/**
	 * Get an unitialized Weather object.
	 */
	public Weather() {
		super();
	}

	/**
	 * Get a string representation of the Weather object.
	 */
	public String toString() {
		String ret="";
		try {
			parseInfo();
			// Deal with not getting our data.
			if(error) {
				ret+=get("ERROR", "An unknown error has occurred");
			} else {
			}
		} catch(Exception e) {
			e.printStackTrace();
			// Just let it return null
		}
		// return(ret);
		// return(hinfo.toString() + "\nLeftovers:\n" + relevent);
		return(shortWeather);
	}

	protected void parseInfo() throws Exception {
		if(hinfo==null) {
			hinfo=new Hashtable();
			hinfo.put("zip_code", arg);
			getInfo();
			String lines[]=SpyUtil.split("\n", info);
			int section=0;
			for(int i=0; i<lines.length; i++) {
				// we really only care about the first section
				if(lines[i].startsWith("CURRENTLY")) {
					section=1;
					i++;  // When it was reported
					i++;  // Status
					hinfo.put("STATUS", lines[i]);
				} else if(section>0
					&& lines[i].startsWith("Temp:")) {
					section=2;
				} else if(section>1
					&& lines[i].startsWith("Detailed Local Forecast")) {
					section=3;
					i++;  // 7-DAY FORECAST
					i++;  // last updated
					i++;  // Start doing days
				} else if(section>2
					&& lines[i].startsWith("Temp. converter")) {
					section=4;
				}

				// Section zero is basically the header before we have the
				// city name
				if(section==0 && city==null) {
					city=lines[i];
					int sds=city.indexOf(" - ");
					city=city.substring(sds+3, city.length());
					shortWeather+=city + "\r\n";
					hinfo.put("city", city);
				} // Section zero
				// Section two is the first information section...we're
				// going to parse it into the hinfo hash, odd lines are
				// keys, even lines are values
				else if(section==2) {
					int colon=lines[i].indexOf(":");
					// Key is the stuff before the colon
					String key=lines[i].substring(0, colon);
					// Move on to the value
					i++;
					String value=stripDeg(lines[i]);

					hinfo.put(key, value);

					shortWeather+=key + ": " + value + "\r\n";
				} // Section two
				else if(section==3) {
					String day=lines[i]; i++;
					String status=lines[i]; i++;
					String hi=stripDeg(lines[i]); i++;
					String lo=stripDeg(lines[i]); i++;

					// Add today's to the short weather.
					if(day.equals("TODAY")) {
						shortWeather+="Today:  " + status + " - "
							+ hi + ", " + lo;
						hinfo.put("FORECAST", status + " - " + hi + ", " +
							lo);
					}

					// This will loop through every element in the
					// forecast, but I'm not quite sure how I want to do
					// this yet.  When I decide, it's important to take the
					// hinfo.put out of the section above.
				} else if(section==4) {
					relevent+=lines[i] + "\n";
				}
			} // For loop through lines
			if(section==0) {
				String error_string="Unable to get weather.  "
					+ "Invalid zip code?";
				hinfo.put("ERROR", error_string);
				shortWeather=error_string;
			} else {
				error=false;
			}
		} // if there's a need to find it at all.
	}

	private String stripDeg(String what) {
		int amp=what.indexOf("&");
		String value=null;
		if(amp>0) {
			value=what.substring(0, amp);
		} else {
			value=what;
		}
		return(value);
	}

	protected void getInfo() throws Exception {
		if(info==null) {
			String url="http://www.weather.com/weather/local/";
			url += arg;
			hinfo.put("URL", url);
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
			System.out.println("Got\n" + info);
			relevent="";
			shortWeather="";
		}
	}

	public static void main(String args[]) throws Exception {
		Weather w = new Weather(args[0]);
		System.out.println("Info:\n" + w);
		System.out.println("Info (XML):\n" + w.toXML());
	}
}
