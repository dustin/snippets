// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Weather.java,v 1.3 2000/03/22 07:49:34 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Get Weather info from weather.com
 */

public class Weather extends Info {

	protected String zip_code=null;
	protected String relevent=null;
	protected String shortWeather=null;
	protected String city=null;

	/**
	 * Get a Weather object.
	 *
	 * @param zip_code zip code to look up weather for.
	 */
	public Weather(String zip_code) {
		super();
		this.zip_code = zip_code;
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
			// Just let it return null
		}
		// return(ret);
		// return(hinfo.toString() + "\nLeftovers:\n" + relevent);
		return(shortWeather);
	}

	protected void parseInfo() throws Exception {
		if(hinfo==null) {
			getInfo();
			hinfo=new Hashtable();
			String lines[]=SpyUtil.split("\n", info);
			int section=0;
			for(int i=0; i<lines.length; i++) {
				// we really only care about the first section
				if(lines[i].startsWith("CURRENTLY")) {
					section=1;
				} else if(section>0
					&& lines[i].startsWith("Temp:")) {
					section=2;
				} else if(section>1
					&& lines[i].startsWith("Detailed Local Forecast")) {
					section=3;
				}

				// Section zero is basically the header before we have the
				// city name
				if(section==0 && city==null) {
					city=lines[i];
					int sds=city.indexOf(" - ");
					city=city.substring(sds+3, city.length());
					shortWeather+=city + "\r\n";
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
					int amp=lines[i].indexOf("&");
					String value=null;
					if(amp>0) {
						value=lines[i].substring(0, amp);
					} else {
						value=lines[i];
					}
					hinfo.put(key, value);

					shortWeather+=key + ": " + value + "\r\n";
				} // Section two
				else if(section==3) {
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

	protected void getInfo() throws Exception {
		if(info==null) {
			String url="http://www.weather.com/weather/us/zips/";
			url += zip_code + ".html";
			HTTPFetch f = new HTTPFetch(url);
			info=f.getStrippedData();
			relevent="";
			shortWeather="";
		}
	}

	public static void main(String args[]) throws Exception {
		Weather w = new Weather(args[0]);
		System.out.println("Info:\n" + w);
	}
}
