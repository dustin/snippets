// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Temperatures.java,v 1.3 2002/11/11 18:22:22 dustin Exp $

package net.spy.rpc.services;

import java.util.*;

import net.spy.temperature.*;

/**
 * XML RPC Interface to temperature gettin'
 */
public class Temperatures extends Remote {

	/**
	 * Get an instance of Temperatures.
	 */
	public Temperatures() {
		super();
	}

	/**
	 * List all known thermometers.
	 */
	public Vector listTherms() throws Exception {
		SpyTemp t=new SpyTemp();
		String therms[]=t.listTherms();

		Vector v=new Vector();
		for(int i=0; i<therms.length; i++) {
			v.addElement(therms[i]);
		}
		return(v);
	}

	/**
	 * Get all temperatures from all thermometers.
	 */
	public Hashtable getTemperatures() throws Exception {
		Hashtable h=new Hashtable();

		SpyTemp t=new SpyTemp();
		String therms[]=t.listTherms();

		for(int i=0; i<therms.length; i++) {
			h.put(therms[i], new Double(t.getTemp(therms[i])));
		}

		return(h);
	}

	/**
	 * Get a specific temperature from a specific thermometer.
	 */
	public double getTemperature(String therm) throws Exception {
		SpyTemp t=new SpyTemp();
		return(t.getTemp(therm));
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		Temperatures t=new Temperatures();

		if(args.length > 0 ) {
			System.out.println(t.getTemperature(args[0]));
		} else {
			System.out.println(t.getTemperatures());
		}
	}

}
