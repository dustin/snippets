/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfig.java,v 1.7 2000/06/20 07:30:21 dustin Exp $
 */

package net.spy;

import java.util.*;
import java.lang.*;
import java.io.*;

/**
 * SpyConfig - an abstracted config file maintainer.
 * <p>
 * Currently, the config file must be in XML, in a specific format.
 * I'll fix that.
 */

public class SpyConfig extends Hashtable {
	protected static Hashtable ConfigStore=null;
	protected static Hashtable ConfigTimeStamps=null;

	/**
	 * Construct a new SpyConfig object describing a config file.
	 *
	 * @param conffile The config file we are describing.
	 */
	public SpyConfig(String conffile) {
		super();
		if(ConfigStore==null) {
			ConfigStore=new Hashtable();
			ConfigTimeStamps=new Hashtable();
		}

		// See whether we've cached the config file or not.
		if(isUpToDate(conffile)) {
			// We've already generated this, set it here.
			set( (Hashtable) ConfigStore.get(conffile) );
		} else {
			try {
				SpyConfigReader r = new SpyConfigReader();
				Hashtable h = r.hashConfig(conffile);
				record(conffile, h);
				set(h);
			} catch(Exception e) {
				// Not necessary
				// System.err.println("Got exception:  " + e);
				// e.printStackTrace();
			}
		}
	}

	// Check to see if we have current data on this file.
	protected boolean isUpToDate(String file) {
		boolean r = false;
		try {
			if(ConfigStore.containsKey(file)) {
				long ts=(long)((Long)ConfigTimeStamps.get(file)).longValue();
				File f = new File(file);
				if(ts == f.lastModified()) {
					r=true;
				}
			}
		} catch(Exception e) {
			// Defaults to false.
		}
		return(r);
	}

	// record stuff to keep up with config file status
	protected void record(String file, Hashtable h) {
		try {
			File f = new File(file);
			Long l = new Long(f.lastModified());

			ConfigStore.put(file, h);
			ConfigTimeStamps.put(file, l);
		} catch(Exception e) {
			// Whatever
		}
	}

	protected void set(Hashtable h) {
		for(Enumeration e = h.keys(); e.hasMoreElements(); ) {
			String key = (String)e.nextElement();
			put(key, h.get(key));
		}
	}

	/**
	 * Get the value for a given config entry.
	 *
	 * @param key which config entry to return.
	 */
	public String get(String key) {
		return( (String)super.get(key));
	}

	/**
	 * Get the value for a given config entry, with default.
	 *
	 * @param key which config entry to return.
	 * @param def default in case the entry doesn't exist.
	 */

	/**
	 * Get an int value for a given config entry.  Please note, a default
	 * is *required* because undefined ints suck.
	 *
	 * @param key which config entry to return.
	 * @param def default in case the entry doesn't exist.
	 */
	public int getInt(String key, int def) {
		String value=get(key);
		int r=def;
		if(value!=null) {
			r=Integer.parseInt(value);
		}
		return(r);
	}

	// Assign a value to a key if there isn't already one.
	protected void orput(String key, String value) {
		if(!containsKey(key)) {
			put(key, value);
		}
	}
}
