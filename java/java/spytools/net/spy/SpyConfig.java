/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfig.java,v 1.2 1999/10/20 07:41:19 dustin Exp $
 */

package net.spy;

import java.util.*;
import java.lang.*;

public class SpyConfig extends Hashtable {
	protected static Hashtable ConfigStore=null;

	public SpyConfig(String conffile) {
		super();
		if(ConfigStore==null) {
			ConfigStore=new Hashtable();
		}

		if(ConfigStore.containsKey(conffile)) {
			// We've already generated this, set it here.
			set( (Hashtable) ConfigStore.get(conffile) );
		} else {
			try {
				SpyConfigReader r = new SpyConfigReader();
				Hashtable h = r.hashConfig(conffile);
				ConfigStore.put(conffile, h);
				set(h);
			} catch(Exception e) {
				// Not necessary
				// System.err.println("Got exception:  " + e);
				// e.printStackTrace();
			}
		}
	}

	protected void set(Hashtable h) {
		for(Enumeration e = h.keys(); e.hasMoreElements(); ) {
			String key = (String)e.nextElement();
			put(key, h.get(key));
		}
	}

	public String get(String key) {
		return( (String)super.get(key));
	}

	// Assign a value to a key if there isn't already one.
	protected void orput(String key, String value) {
		if(!containsKey(key)) {
			put(key, value);
		}
	}
}
