// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: HTTPFetch.java,v 1.1 1999/11/26 05:36:45 dustin Exp $

package net.spy.net;

import java.net.*;
import java.io.*;
import java.util.*;

public class HTTPFetch {
	URL url;

	public HTTPFetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	public Vector getLines() throws Exception {
		Vector v = new Vector();

		try {
			URLConnection uc = url.openConnection();

			InputStream i = uc.getInputStream();
			BufferedReader br =
				new BufferedReader( new InputStreamReader(i));

			String line;
			while( (line=br.readLine()) != null) {
				v.addElement(line);
			}

		} catch(Exception e) {
			throw new Exception(e.getMessage());
		}
		return(v);
	}
}
