/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfigReader.java,v 1.3 2000/03/27 11:17:35 dustin Exp $
 */

package net.spy;

import java.lang.*;
import java.util.*;
import java.io.*;

/**
 * This object is specific to IBM's XML parser.
 */

public class SpyConfigReader {

	public Hashtable hashConfig(String filename) throws Exception {
		Properties p = new Properties();
		p.load(new FileInputStream(filename));
		return(p);
	}
}
