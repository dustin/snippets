/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfig.java,v 1.10 2000/06/28 23:22:35 dustin Exp $
 */

package net.spy;

import java.util.*;
import java.lang.*;
import java.io.*;

/**
 * SpyConfig - an abstracted config file maintainer.
 *
 * The current config file format is that of a java Properties file.  This
 * class makes it easier to load them, caches them, and gives a good base
 * for extensions that load default config files for projects that are hard
 * to pass config filepaths into.
 */

public class SpyConfig extends Hashtable {
	protected static Hashtable configStore=null;
	protected static Hashtable configTimeStamps=null;

	/**
	 * Construct a new SpyConfig object describing a config file.
	 *
	 * @param conffile The config file we are describing.
	 */
	public SpyConfig(String conffile) {
		super();

		confInit();
		loadConfig(conffile);
	}

	/**
	 * Construct a new SpyConfig object without a config file.
	 */
	public SpyConfig() {
		super();

		confInit();
	}

	protected synchronized void confInit() {
		if(configStore==null) {
			configStore=new Hashtable();
			configTimeStamps=new Hashtable();
		}
	}

	/**
	 * Try to load a config file.  This function exists primarily for
	 * classes that extend SpyConfig and want to have multiple default
	 * locations for config files.
	 *
	 * @param conffile Path to the configuration file to load.
	 *
	 * @return true if the config loaded successfully.
	 */
	public boolean loadConfig(String conffile) {
		boolean loaded=false;

		// See whether we've cached the config file or not.
		if(isUpToDate(conffile)) {
			// We've already generated this, set it here.
			set( (Hashtable) configStore.get(conffile) );
			loaded=true;
		} else {
			try {
				SpyConfigReader r = new SpyConfigReader();
				Hashtable h = r.hashConfig(conffile);
				record(conffile, h);
				set(h);
				loaded=true;
			} catch(Exception e) {
			}
		}

		return(loaded);
	}

	// Check to see if we have current data on this file.
	protected boolean isUpToDate(String file) {
		boolean r = false;
		try {
			if(configStore.containsKey(file)) {
				long ts=(long)((Long)configTimeStamps.get(file)).longValue();
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

			configStore.put(file, h);
			configTimeStamps.put(file, l);
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
	public String get(String key, String def) {
		String ret=get(key);
		if(ret==null) {
			ret=def;
		}
		return(ret);
	}

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

	/**
	 * Assign a value to the config only if it doesn't already exist.
	 * Useful for setting defaults.
	 *
	 * @param key config key
	 * @param value config value
	 *
	 */

	public void orput(String key, String value) {
		if(!containsKey(key)) {
			put(key, value);
		}
	}
}
