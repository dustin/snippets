// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.5 2000/01/25 06:49:53 dustin Exp $

package net.spy;

/**
 * An entry in the spy log.
 */

public class SpyLogEntry extends Object {
	public SpyLogEntry() {
		// Do nothing.
	}

	/**
	 * toString <i>must</i> be overridden for this to be useful.
	 */
	public String toString() {
		return("ERROR:  Method should be overridden");
	}
}
