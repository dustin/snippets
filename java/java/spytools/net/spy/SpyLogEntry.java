// Copyright (c) 1999 Dustin Sallings
//
// $Id: SpyLogEntry.java,v 1.4 2000/01/25 04:39:28 dustin Exp $

package net.spy;

public class SpyLogEntry extends Object {
	public SpyLogEntry() {
		// Do nothing.
	}

	// This really needs to be overridden.
	public String toString() {
		return("ERROR:  Method should be overridden");
	}
}
