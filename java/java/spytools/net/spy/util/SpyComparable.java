// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyComparable.java,v 1.2 2001/04/26 07:22:00 dustin Exp $

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
	int compare(Object obj1, Object obj2);
}
