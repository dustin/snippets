// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: IOTimeoutException.java,v 1.2 2002/07/10 04:25:55 dustin Exp $

package net.spy.net;

import java.net.ConnectException;

/**
 * Exception thrown when IOs timeout.
 */
public class IOTimeoutException extends ConnectException {

	/**
	 * Get an instance of IOTimeoutException.
	 */
	public IOTimeoutException(String message) {
		super(message);
	}
}
