/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfigReader.java,v 1.2 2000/01/24 06:40:28 dustin Exp $
 */

package net.spy;

import java.lang.*;
import java.util.*;
import java.io.*;

import org.w3c.dom.*;

/**
 * This object is specific to IBM's XML parser.
 */

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
