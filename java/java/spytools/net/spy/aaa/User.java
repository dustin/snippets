// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: User.java,v 1.1 2001/07/10 09:22:44 dustin Exp $

package net.spy.aaa;

import net.spy.util.*;

/**
 * Represents an authenticatable user.
 */
public class User extends Object {

	private String username=null;
	private String pwhash=null;

	/**
	 * Get an instance of User.
	 *
	 * @param username The user's username
	 * @param pwhash The user's hashed password
	 */
	public User(String username, String pwhash) {
		super();
	}

	/**
	 * Get the username.
	 */
	public String getUsername() {
		return(username);
	}

	/**
	 * Check the password.
	 */
	public boolean checkPassword(String pw) {
		Digest d=new Digest();
		return(d.checkPassword(pw, pwhash));
	}

}
