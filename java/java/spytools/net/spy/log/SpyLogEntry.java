// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.1 2000/07/19 03:22:33 dustin Exp $

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
