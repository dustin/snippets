// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.2 2000/01/24 06:40:33 dustin Exp $

package net.spy;

/**
 * A SpyLogEntry.  The SpyLogEntry class is what is passed into SpyLog to
 * be logged.
 * <p>
 * This class *must* be overridden to be useful, specifically toString().
 */

public class SpyLogEntry extends Object {
	public SpyLogEntry() {
		// Do nothing.
	}

	/**
	 * Prepare a LogEntry for representation in the log store.
	 *
	 * @return SpyLog-ready representation of the LogEntry, for example, a
	 * SQL query if the SpyLog store is a SQL database.
	 */
	public String toString() {
		return("ERROR:  Method should be overridden");
	}
}
