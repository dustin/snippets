// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: IOTimeoutException.java,v 1.1 2001/07/02 23:56:55 dustin Exp $

package net.spy.net;

import java.net.*;

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
