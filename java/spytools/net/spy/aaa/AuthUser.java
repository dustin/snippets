// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: AuthUser.java,v 1.2 2002/07/10 04:24:47 dustin Exp $

package net.spy.aaa;

/**
 * A user capable of checking its own password.  Used internally by the
 * Authenticator.
 */
public interface AuthUser {

	/**
	 * Check a password for this user.
	 */
	void checkPassword(String password) throws AuthException;

}

