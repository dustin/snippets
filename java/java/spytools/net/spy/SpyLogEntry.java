// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.3 2000/01/24 10:10:47 dustin Exp $

package net.spy;

/**
 * A SpyLogEntry.  Things should look like this to be logged.
 */

public interface SpyLogEntry {
	/**
	 * Prepare a LogEntry for representation in the log store.
	 *
	 * @return SpyLog-ready representation of the LogEntry, for example, a
	 * SQL query if the SpyLog store is a SQL database.
	 */
	public String toString();
}
