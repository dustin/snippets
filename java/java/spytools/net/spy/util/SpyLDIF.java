// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: SpyLDIF.java,v 1.1 2000/07/25 21:52:58 dustin Exp $

package net.spy.util;

import java.util.*;
import sun.misc.*;

public class SpyLDIF extends Hashtable {
	public SpyLDIF(String ldif) {
		super();

		parseLDIF(ldif);
	}

	protected void parseLDIF(String ldif) {
		// Vector v=new Vector();
		StringTokenizer onlines=new StringTokenizer(ldif, "\r\n");
		String chunk="";

		while(onlines.hasMoreTokens()) {
			String line=onlines.nextToken();

			// If this line starts with a space, it's a continuation
			if(line.startsWith(" ")) {
				chunk+=line.trim();
			} else {
				if(chunk.length()>0) {
					boolean decode=true;
					int colon=chunk.indexOf("::");
					if(colon<0) {
						decode=false;
						colon=chunk.indexOf(":");
					}

					// Only process the segment if it's valid.
					if(colon>0) {
						String k=chunk.substring(0, colon);
						String v=chunk.substring(colon+1).trim();

						if(decode) {
							try {
								String tmp=v.substring(1).trim();
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
				chunk=line;
			} // End of else
		} // Read all tokens.
	} // parseLDIF
}
