// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: SpyLDIFReader.java,v 1.4 2002/08/21 00:53:21 dustin Exp $

package net.spy.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

/**
 * Reader of .ldif files.
 */
public class SpyLDIFReader extends BufferedReader {

	/**
	 * Create a SpyLDIFReader stream that uses a default-sized
	 * input buffer.
	 */
	public SpyLDIFReader(Reader r) {
		super(r);
	}

	/**
	 * Create a SpyLDIFReader stream that uses the given input buffer size
	 */
	public SpyLDIFReader(Reader r, int bufsize) {
		super(r, bufsize);
	}

	/**
	 * Get the next ldif entry from the file.
	 */
	public SpyLDIF readLDIF() throws IOException {
		StringBuffer data=new StringBuffer(1024);
		String tmp=null;
		SpyLDIF ldif=null;
		boolean done=false;
		int linecount=0;

		while(!done) {
			try {
				tmp=readLine();
				linecount++;
				if(linecount>1000) {
					throw new
						IOException("Too many lines in LDIF entry, max 1000");
				}
				if(tmp!=null) {
					if(tmp.length()<2) {
						// This is an empty line, we're done.
						done=true;
					}
					data.append(tmp);
					data.append("\n");
				}
			} catch(IOException e) {
				// This just tells us that we're at the end.
				done=true;
			}
		}

		if(data.length() == 0 ) {
			ldif=null;
		} else {
			ldif=new SpyLDIF(data.toString());
		}

		return(ldif);
	}
}
