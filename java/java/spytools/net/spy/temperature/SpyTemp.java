// Copyright (c) 2000  Spy Internetworking
//
// $Id: SpyTemp.java,v 1.1 2000/01/18 23:41:14 dustin Exp $

package net.spy.temperature;

import java.util.*;
import java.lang.*;

import net.spy.net.*;

public class SpyTemp extends Object {

	static String temp_base="http://bleu.west.spy.net/servlet/"
		+ "net.spy.temperature.Temperature";

	public SpyTemp() {
		super();
	}

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
