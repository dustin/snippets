// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: SpyLDIF.java,v 1.11 2001/04/03 00:02:03 dustin Exp $

package net.spy.util;

import java.util.*;
import java.io.*;
import net.spy.SpyUtil;

public class SpyLDIF extends Hashtable {

	/**
	 * Get a new SpyLDIF object that's not initialized
	 */
	public SpyLDIF() {
		super();
	}

	/**
	 * Get a new SpyLDIF object from a Hashtable
	 */
	public SpyLDIF(Hashtable h) {
		super();

		for(Enumeration e=h.keys(); e.hasMoreElements(); ) {
			// Cast the key and value to a string because we reallyreally
			// need strings.
			String k=(String)e.nextElement();
			String v=(String)h.get(k);
			put(k, v);
		}
	}

	/**
	 * Return a new SpyLDIF object from the passed in LDIF entry
	 */
	public SpyLDIF(String ldif_entry) {
		super();

		parseLDIFEntry(ldif_entry);
	}

	/**
	 * Parse the data from an LDIF file, may include multiple entries.
	 */
	public static Vector parseLDIF(Reader r) {
		Vector v=new Vector();
		SpyLDIFReader slr=new SpyLDIFReader(r);
		SpyLDIF ld=null;
		boolean done=false;

		while(!done) {
			ld=null;
			try {
				ld=slr.readLDIF();
			} catch(Exception e) {
				System.err.println("Error reading LDIF:  " + e);
				e.printStackTrace();
			}
			if(ld!=null) {
				v.addElement(ld);
			} else {
				done=true;
			}
		}

		return(v);
	}

	/**
	 * Get a string value from the LDIF entry
	 */
	public String getString(String key) {
		String res=(String)super.get(key);
		return(res);
	}

	/**
	 * Get an int value from the LDIF entry
	 */
	public int getInt(String key) {
		String res=(String)super.get(key);
		return(Integer.parseInt(res));
	}

	/**
	 * Get LDIF text from this LDIF object
	 */
	public String getLDIF() {
		String ret="";
		Base64 base64=new Base64();

		for(Enumeration e=keys(); e.hasMoreElements(); ) {
			String k=null, v=null;
			k=(String)e.nextElement();

			// Don't do the encoded stuff
			if(k.indexOf(":encoded")==-1) {
				v=getString(k);
				String v2=base64.encode(v.getBytes());
				String parts[]=SpyUtil.split("\r\n\t ", v2);
				if(parts.length>0) {
					v2=parts[0] + "\n";
					for(int i=1; i<parts.length; i++) {
						v2+="  " + parts[i] + "\n";
					}
				}
				ret+=k + ":: " + v2;
			}
		}

		return(ret);
	}

	private void decodeAndStore(String chunk) {
		boolean decode=true;
		int space=chunk.indexOf(" ");
		int colon=chunk.indexOf(":: ");
		if(colon<0 || colon>space) {
			decode=false;
			colon=chunk.indexOf(": ");
		}
		// Only process the segment if it's valid.
		if(colon>0) {
			String k=chunk.substring(0, colon);
			String v=chunk.substring(colon+1).trim();

			if(decode) {
				try {
					String tmp=v.substring(1).trim();
					this.put(k + ":encoded", tmp);
					Base64 base64 = new Base64();
					byte data[]=base64.decode(tmp);
					v=new String(data);
				} catch(Exception e) {
					System.err.println(
						"LDIF: error while decoding: " + e);
				}
			}
			this.put(k, v);
		}
	}

	private void parseLDIFEntry(String ldif) {
		StringTokenizer onlines=new StringTokenizer(ldif, "\r\n");
		String chunk="";

		while(onlines.hasMoreTokens()) {
			String line=onlines.nextToken();

			// If this line starts with a space, it's a continuation
			if(line.startsWith(" ")) {
				chunk+=line.trim();
			} else {
				if(chunk.length()>0) {
					decodeAndStore(chunk);
				}
				chunk=line;
			} // End of else
		} // Read all tokens.
		if(chunk.length()>0) {
			decodeAndStore(chunk);
		}
	} // parseLDIFEntry
}
