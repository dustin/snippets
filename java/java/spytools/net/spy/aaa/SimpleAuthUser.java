// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SimpleAuthUser.java,v 1.1 2001/07/11 09:30:51 dustin Exp $

package net.spy.aaa;

import net.spy.util.Digest;

/**
 * A simple implementation of AuthUser for that contains its own hashed
 * password and can deal with checking it.
 */
public class SimpleAuthUser extends Object implements AuthUser {

	private String hashedPassword=null;

	/**
	 * Get an instance of SimpleAuthUser.
	 *
	 * @param hp the hashed password for this user
	 */
	public SimpleAuthUser(String hp) {
		super();
		this.hashedPassword=hp;
	}

	/**
	 * Check the hashed password against the stored password.
	 */
	public void checkPassword(String password) throws AuthException {
		Digest d=new Digest();
		if(!d.checkPassword(password, hashedPassword)) {
			throw new AuthException("Invalid password.");
		}
	}

}
