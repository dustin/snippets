// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Info.java,v 1.6 2001/02/07 06:31:14 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * net.spy.info.* superclass -- extend this to provide info services
 */

public abstract class Info extends Object {

	// Error if the thing doesn't believe it did what it should.
	protected boolean error=true;

	// Info store.
	protected String info=null;
	protected Hashtable hinfo=null;
	protected String arg=null;

	/**
	 * Get an info object.
	 */
	public Info() {
		super();
	}

	/**
	 * Set the arguments, useful when it was called with a constructor with no
	 * args.
	 */
	public void setArg(String arg) {
		this.arg=arg;
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

	// Make sure we only have letters and numbers in the tag.
	private String xmlSafe(String key) {
		String ret="";
		key=key.toLowerCase();
		char chars[]=key.toCharArray();

		for(int i=0; i<chars.length; i++) {
			if(Character.isLetterOrDigit(chars[i])) {
				ret+=chars[i];
			}
		}
		return(ret);
	}

	/**
	 * Get an XML representation of the info object as a String.
	 */
	public String toXML() {
		String ret="";
		try {
			if(hinfo==null) {
				parseInfo();
			}
			for(Enumeration e = hinfo.keys(); e.hasMoreElements(); ) {
				String key=(String)e.nextElement();
				String safekey=xmlSafe(key);
				String data=(String)hinfo.get(key);
				ret+="<" + safekey + ">\n"
					+ "\t" + data + "\n"
					+ "</" + safekey + ">\n";
			}
		} catch(Exception e) {
			// Just return an empty string
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
	 *
	 * @exception Exception will be thrown if the info cannot be parsed
	 */
	public String get(String what) throws Exception {
		if(hinfo==null) {
			parseInfo();
		}
		String ret=(String) hinfo.get(what);
		return(ret);
	}

	/**
	 * Same as the above, but allows a default to use when the variable
	 * does not exist.
	 *
	 * @param what which variable to get
	 * @param def default value
	 *
	 * @exception Exception will be thrown if the info cannot be parsed
	 */
	public String get(String what, String def) throws Exception {
		String ret=(String) get(what);
		if(ret==null) {
			ret=def;
		}
		return(ret);
	}

	/**
	 * getInfo gets the data to be parsed.
	 */
	private abstract void getInfo() throws Exception;

	/**
	 * parseInfo parses the data into the hinfo Hashtable object.
	 * It will be called when needed.
	 */
	private abstract void parseInfo() throws Exception;
}
