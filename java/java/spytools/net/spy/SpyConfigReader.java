/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfigReader.java,v 1.1 1999/10/20 07:58:12 dustin Exp $
 */

package net.spy;

import java.lang.*;
import java.util.*;
import java.io.*;

import org.w3c.dom.*;

public class SpyConfigReader {

	public Hashtable hashConfig(String filename) throws Exception {
		// SpyParser parser = (SpyParser)new SpyParser();
		com.ibm.xml.parsers.DOMParser parser =
			new com.ibm.xml.parsers.DOMParser();
		parser.parse(filename);
		Document document = parser.getDocument();
		NodeList elements = document.getElementsByTagName("entry");
		Hashtable h = new Hashtable();

		for(int i=0; i<elements.getLength(); i++) {
			Element node = (Element)elements.item(i);
			h.put(node.getAttribute("name"), node.getAttribute("value"));
		}
		return(h);
	}
}
