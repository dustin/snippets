// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SimpleCallbackHandler.java,v 1.1 2001/07/10 09:22:42 dustin Exp $

package net.spy.aaa;

import javax.security.*;
import javax.security.auth.*;
import javax.security.auth.callback.*;

/**
 * A simple callback handler for doing password authentication given a
 * username and password.
 */
public class SimpleCallbackHandler extends Object implements CallbackHandler {

	private String user=null;
	private String password=null;

	/**
	 * Get an instance of SimpleCallbackHandler.
	 */
	public SimpleCallbackHandler(String user, String password) {
		super();
		this.user=user;
		this.password=password;
	}

	/**
	 * Handle the callbacks.  Currently only NameCallback and
	 * PasswordCallback are handled, the handler simply places the values
	 * into the callbacks.  No magic here.
	 *
	 * @param callbacks the callbacks that want data
	 *
	 * @exception UnsupportedCallbackException if you pass something other
	 * than a NameCallback or PasswordCallback
	 */
	public void handle(Callback[] callbacks)
		throws UnsupportedCallbackException {

		for(int i=0; i<callbacks.length; i++) {
			if(callbacks[i] instanceof NameCallback) {
				NameCallback nc=(NameCallback)callbacks[i];
				nc.setName(user);
			} else if(callbacks[i] instanceof PasswordCallback) {
				PasswordCallback pc=(PasswordCallback)callbacks[i];
				char data[]=new char[password.length()];
				password.getChars(0, password.length(), data, 0);
				pc.setPassword(data);
			} else {
				throw new UnsupportedCallbackException(callbacks[i],
					"What's this?");
			}
		}
	}
}
