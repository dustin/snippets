// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Authable.java,v 1.1 2001/07/11 09:30:49 dustin Exp $

package net.spy.aaa;

/**
 * Authable things.
 */
public interface Authable {

	/**
	 * Get the username who's trying to auth.
	 */
	String getUsername();

}
