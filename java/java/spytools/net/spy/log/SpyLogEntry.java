// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.2 2002/07/10 04:25:49 dustin Exp $

package net.spy.log;

/**
 * An entry in the spy log.
 */

public class SpyLogEntry extends Object {
	public SpyLogEntry() {
		super();
	}

	/**
	 * toString <i>must</i> be overridden for this to be useful.
	 */
	public String toString() {
		return("ERROR:  Method should be overridden");
	}
}

