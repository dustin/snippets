// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyComparable.java,v 1.1 2000/09/05 08:04:02 dustin Exp $

package net.spy.util;

/**
 * SpyComparible allows a class to describe how two objects should be
 * compared.  One of these must be implemented for each type of object
 * we'll be sorting.
 */

public interface SpyComparable {
	/**
	 * Compare two objects.
	 */
	public int compare(Object obj1, Object obj2);
}
