// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>

package net.spy.net;

import java.net.*;
import java.io.*;
import java.util.*;

// Fetch the contents of a URL
public class HTTPFetch {
	URL url;

	public HTTPFetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	// Get each line in its own vector.
	public Vector getLines() throws Exception {
		Vector v = new Vector();

		try {
			BufferedReader br = getReader();
			String line;
			while( (line=br.readLine()) != null) {
				v.addElement(line);
			}
		} catch(Exception e) {
			throw new Exception(e.getMessage());
		}

		return(v);
	}

	// Get all the data as one big string.
	public String getData() throws Exception {
		String s = "";

		try {
			BufferedReader br = getReader();
			String line;
			while( (line=br.readLine()) != null) {
				s+=line + "\n";
			}
		} catch(Exception e) {
			throw new Exception(e.getMessage());
		}
		return(s);
	}

	// Get a reader for the above two routines.
	protected BufferedReader getReader() throws Exception {
		URLConnection uc = url.openConnection();
		InputStream i = uc.getInputStream();
		BufferedReader br =
			new BufferedReader( new InputStreamReader(i));
		return(br);
	}
}
