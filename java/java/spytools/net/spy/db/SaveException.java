// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SaveException.java,v 1.1 2002/07/22 06:11:45 dustin Exp $

package net.spy.db;

import net.spy.util.NestedException;

/**
 * Exception thrown when a Saver save fails.
 */
public class SaveException extends NestedException {

	/**
	 * Get an instance of SaveException with a message.
	 */
	public SaveException(String msg) {
		super(msg);
	}

	/**
	 * Get an instance of SaveException with a message and a root cause.
	 */
	public SaveException(String msg, Throwable root) {
		super(msg, root);
	}

}
