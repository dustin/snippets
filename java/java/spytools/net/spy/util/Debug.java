// Copyright (c) 2001  Beyond.com <dustin@beyond.com>
//
// $Id: Debug.java,v 1.3 2001/07/03 08:59:24 dustin Exp $

package net.spy.util;

import java.util.*;
import java.io.*;

/**
 * Generic Debug logger.  Logs to a file that's listed in a System
 * property, or does not log if the System property is not set.
 */
public class Debug extends Object {

	// Storage for all the file thingies
	private static String debug_mutex="debug_mutex";
	private static Hashtable debugs=null;
	private String propname=null;

	/**
	 * Get a new Debug object that writes its output to the Fle specified
	 * in the given System property.
	 */
	public Debug(String propname) {
		super();
		this.propname=propname;
		init();
	}

	private void init() {
		synchronized(debug_mutex) {
			if(debugs==null) {
				debugs=new Hashtable();
			}

			// If we don't have the debugstream, try to get it.
			if(debugs.get(propname)==null) {

				String of=System.getProperty(propname);

				if(of!=null) {
					try {
						FileOutputStream fos=new FileOutputStream(of);
						PrintWriter debugOut=new PrintWriter(fos, true);
						debugOut.println(getTimestamp() + ":  Initialized.");
						debugs.put(propname, debugOut);
					} catch(Exception e) {
						System.err.println("Cannot initialize debugging:  "
							+ e);
						e.printStackTrace();
					}
				} // Got a filename
			} // get a prop name
		} // synchronized()
	} // init()

	// Get a timestamp for logging
	private static String getTimestamp() {
		return("[" + new Date().toString() + "]");
	}

	/**
	 * Log a debug message.
	 */
	public void debug(String msg) {
		PrintWriter debugOut=(PrintWriter)debugs.get(propname);
		if(debugOut!=null) {
			String tmsg=getTimestamp() + " " + msg;
			synchronized(debugOut) {
				debugOut.println(tmsg);
				debugOut.flush();
			}
		}
	}

	/**
	 * Close a file when this object goes away.
	 */
	public void finalize() throws Throwable {
		synchronized(debug_mutex) {
			PrintWriter debugOut=(PrintWriter)debugs.get(propname);
			if(debugOut!=null) {
				debugOut.close();
				debugs.remove(propname);
			}
		}
		super.finalize();
	}

}
