// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.6 2000/04/22 10:29:14 dustin Exp $

package net.spy;

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
