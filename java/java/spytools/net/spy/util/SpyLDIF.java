// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: SpyLDIF.java,v 1.6 2000/09/05 08:07:13 dustin Exp $

package net.spy.util;

import java.util.*;
import java.io.*;
import sun.misc.*;

public class SpyLDIF extends Hashtable {

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

	protected void decodeAndStore(String chunk) {
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
					BASE64Decoder base64 = new BASE64Decoder();
					byte data[]=base64.decodeBuffer(tmp);
					v=new String(data);
				} catch(Exception e) {
					System.err.println(
						"LDIF: error while decoding: " + e);
				}
			}
			this.put(k, v);
		}
	}

	protected void parseLDIFEntry(String ldif) {
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
