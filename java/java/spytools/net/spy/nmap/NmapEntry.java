// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: NmapEntry.java,v 1.3 2002/07/10 04:26:00 dustin Exp $

package net.spy.nmap;

import java.util.Enumeration;
import java.util.Hashtable;

import net.spy.SpyUtil;

public class NmapEntry extends Object {
	// The actual line of data
	private String inputLine=null;

	// The entries
	private Hashtable sections=null;

	// The port entries, by port
	private Hashtable _ports=null;

	/**
	 * Construct an NmapEntry object from a line in an nmap machine
	 * readable output file.
	 *
	 * @param inputLine a single line from an nmap machine readable output
	 * file.
	 */
	public NmapEntry(String inputLine) {
		super();
		this.inputLine=inputLine;
		sections=new Hashtable();
		parse();
	}

	/**
	 * gets the IP address for this entry.
	 *
	 * @return The string form of the IP address, or null if it couldn't
	 * figure one out.
	 */
	public String getIP() {
		String ret=null;

		// Do a try block to catch any null pointer exceptions that might
		// come up.  There *really* shouldn't be any.
		try {
			String blah[]=SpyUtil.split(" ", (String)sections.get("Host"));
			ret=blah[0];
		} catch(Exception e) {
			// We'll just return null.
		}

		return(ret);
	}

	/**
	 * gets an Enumeration of NmapPort entries from this entry
	 */
	public Enumeration ports() {
		parsePorts();
		return(_ports.elements());
	}

	/**
	 * gets an NmapPort entry for a single port number
	 *
	 * @param p port number to look up
	 *
	 * @return NmapPort entry describing this machine's binding to that
	 * port, or null if there is no such entry.
	 */
	public NmapPort port(int p) {
		parsePorts();
		Integer n=new Integer(p);
		return((NmapPort)_ports.get(n));
	}

	public String toString() {
		String ret=null;

		ret=getIP() + "\n";

		for(Enumeration e=ports(); e.hasMoreElements(); ) {
			NmapPort p = (NmapPort)e.nextElement();
			ret+="\t" + p.port() + " (" + p.service() + ") - " + p.status()
				+ "\n";
		}

		return(ret);
	}

	// Parse the ports out of the section hash
	private void parsePorts() {
		// Bypass if it's already been called.
		if(_ports!=null) {
			return;
		}

		_ports=new Hashtable();
		try {
			String tmp=(String)sections.get("Ports");
			String a[]=SpyUtil.split(", ", tmp);
			for(int i=0; i<a.length; i++) {
				NmapPort p = new NmapPort(SpyUtil.split("/", a[i]));
				Integer pn=new Integer(p.port());
				_ports.put(pn, p);
			}
		} catch(Exception e) {
		}
	}

	// parse the passed in line.
	private void parse() {
		// Split into the tabed out shite.
		String sections_str[]=SpyUtil.split("\t", inputLine);
		for(int i=0; i<sections_str.length; i++) {
			// Split into name:value
			String pair[]=SpyUtil.split(":", sections_str[i]);

			// Store the section, starting one character away.
			sections.put(pair[0], pair[1].substring(1));
		}
	}
}
