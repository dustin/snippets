// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Authenticator.java,v 1.2 2002/07/10 04:24:51 dustin Exp $

package net.spy.aaa;

/**
 * Perform the actual authentication and generate a ticket, used internally
 * by the SpySecurityManager.
 */
public abstract class Authenticator extends Object {

	/**
	 * Get an instance of Authenticator.
	 */
	public Authenticator() {
		super();
	}

	/**
	 * Lookup a user by Authable.
	 */
	public abstract AuthUser getUser(Authable a) throws AuthException;

	/**
	 * Get a Token for Authenticating a user.
	 */
	public Token authUser(Authable a, String password) throws AuthException {
		AuthUser au=getUser(a);
		au.checkPassword(password);
		return new Token(a.getUsername());
	}

}

