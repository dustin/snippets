// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SaveContext.java,v 1.1 2002/07/22 06:11:45 dustin Exp $

package net.spy.db;

import java.util.Hashtable;

/**
 * Context shared by all Savables inside a Saver.  This lets the Savables
 * keep track of whatever state they may need in order to maintain key
 * integrity or whatever.
 */
public class SaveContext extends Hashtable {

	/**
	 * Get an instance of SaveContext.
	 */
	public SaveContext() {
		super();
	}

}
