/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: SpyConfig.java,v 1.1 1999/10/20 02:25:35 dustin Exp $
 */

package net.spy;

import java.util.*;
import java.lang.*;

public class SpyConfig extends Hashtable {
	public SpyConfig(String conffile) {
		super();
	}

	public String get(String key) {
		return( (String)super.get(key));
	}

	// Assign a value to a key if there isn't already one.
	protected void orput(String key, String value) {
		if(!containsKey(key)) {
			put(key, value);
		}
	}
}
