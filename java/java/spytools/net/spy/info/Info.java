// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Info.java,v 1.1 2000/03/22 07:49:32 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * net.spy.info.* superclass -- extend this to provide info services
 */

public class Info extends Object {

	// Error if the thing doesn't believe it did what it should.
	protected boolean error=true;

	// Info store.
	protected String info=null;
	protected Hashtable hinfo=null;

	/**
	 * Get an info object.
	 */
	public Info() {
		super();
	}

	/**
	 * Get a string representation of the info object.
	 */
	public String toString() {
		String ret="";
		try {
			parseInfo();
			// Deal with not getting our data.
			if(error) {
				ret+=get("ERROR", "An unknown error has occurred");
			} else {
				ret+=info;
			}
		} catch(Exception e) {
			// Just let it return null
		}
		return(ret);
	}

	/**
	 * returns true if the information is good
	 */
	public boolean goodInfo() {
		return(!error);
	}

	/**
	 * Get a value out of the info hash.
	 *
	 * @param what which variable to get
	 */
	public String get(String what) throws Exception {
		parseInfo();
		String ret=(String) hinfo.get(what);
		return(ret);
	}

	/**
	 * Same as the above, but allows a default to use when the variable
	 * does not exist.
	 *
	 * @param what which variable to get
	 * @param def default value
	 */
	public String get(String what, String def) throws Exception {
		parseInfo();
		String ret=(String) hinfo.get(what);
		if(ret==null) {
			ret=def;
		}
		return(ret);
	}

	// This needs to be overridden
	protected void parseInfo() throws Exception {
		throw new Exception("parseInfo() must be overridden!");
	}

	protected void getInfo() throws Exception {
		throw new Exception("getInfo() must be overridden!");
	}
}
