// Copyright (c) 2000  Spy Internetworking
//
// $Id: SpyTemp.java,v 1.5 2002/05/04 05:45:08 dustin Exp $

package net.spy.temperature;

import java.util.*;

import net.spy.net.*;

/**
 * A simple interface to my temperature servlet at home.
 */

public class SpyTemp extends Object {

	private static final String DEFAULT_TEMP_BASE=
		"http://bleu.west.spy.net/servlet/Temperature";

	private String temp_base=null;

	/**
	 * Get a SpyTemp object reading from the default temperature base.
	 */
	public SpyTemp() {
		this(DEFAULT_TEMP_BASE);
	}

	/**
	 * Get a SpyTemp object reading from the default temperature base.
	 *
	 * @param tempBase URL to the net.spy.temperature.Temperature servlet
	 * that will be answering temperature requests.
	 */
	public SpyTemp(String tempBase) {
		super();
		this.temp_base=tempBase;
	}

	/**
	 * List all available thermometers.
	 *
	 * @return list of available thermometers
	 *
	 * @exception Exception if there's a failure getting the list.
	 */
	public String[] listTherms() throws Exception {
		String ret[]=null;
		try {
			HTTPFetch f = new HTTPFetch(temp_base);
			Vector v=f.getLines();
			ret=new String[v.size()];
			for(int i=0; i<v.size(); i++) {
				ret[i]=(String)v.elementAt(i);
			}
		} catch(Exception e) {
			throw new Exception("Error listing therms:  " + e);
		}
		return(ret);
	}

	/**
	 * Get the current reading for a given thermometer.
	 *
	 * @param which which thermometer (from the above list) to read
	 *
	 * @exception Exception if there's a problem getting the reading.
	 */
	public double getTemp(String which) throws Exception {
		String tmpurl= temp_base + "?temp=" + which;
		String s=null;
		double t;

		try {
			HTTPFetch f = new HTTPFetch(tmpurl);
			s=f.getData();
		} catch(Exception e) {
			throw new Exception ("Error getting temperature:  " + e);
		}

		// Make it look like a Double
		s=s.trim();
		s+="d";

		t=Double.valueOf(s).doubleValue();

		// Ugly rounding
		int temptmp=(int)(t*100.0);
		t=(double)temptmp/100;

		return(t);
	}
}
