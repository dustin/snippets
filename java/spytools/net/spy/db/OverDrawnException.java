// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: OverDrawnException.java,v 1.1 2002/08/23 07:46:24 dustin Exp $

package net.spy.db;

/**
 * Exception thrown from KeyStore when too many keys are requested.
 */
public class OverDrawnException extends Exception {

	/**
	 * Get an instance of OverDrawnException.
	 */
	public OverDrawnException() {
		super();
	}

}
