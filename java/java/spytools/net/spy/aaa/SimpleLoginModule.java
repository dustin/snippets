// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SimpleLoginModule.java,v 1.1 2001/07/10 09:22:43 dustin Exp $

package net.spy.aaa;

import java.util.*;
import java.io.IOException;
import javax.security.auth.*;
import javax.security.auth.callback.*;
import javax.security.auth.login.*;
import javax.security.auth.spi.*;

/**
 * A simple login module for username and password login stuff.
 */
public class SimpleLoginModule extends Object {

	private Subject subject=null;
	private CallbackHandler callbackHandler=null;
	private Map sharedState=null;
	private Map options=null;

	private boolean authed=false;
	private boolean committed=false;

	private String username=null;
	private String password=null;

	private SpyPrincipal principal=null;

	/**
	 * Get an instance of SimpleLoginModule.
	 */
	public SimpleLoginModule() {
		super();
	}

	/**
	 * Initialize the callback handler.
	 */
	public void initialize(Subject subject, CallbackHandler cbh,
		Map sharedState, Map options) {

		this.subject=subject;
		this.callbackHandler=cbh;
		this.sharedState=sharedState;
		this.options=options;
	}

	/**
	 * Perform the actual login.
	 */
	public boolean login() throws LoginException {
		Callback callbacks[]=new Callback[2];
		callbacks[0]=new NameCallback("Username");
		callbacks[1]=new PasswordCallback("Password", false);

		try {
			callbackHandler.handle(callbacks);
		} catch(IOException ioe) {
			throw new LoginException("IOException logging in:  " + ioe);
		} catch(UnsupportedCallbackException uce) {
			throw new LoginException("Unsupported callback " + uce);
		}

		username=((NameCallback)callbacks[0]).getName();
		char p[]=((PasswordCallback)callbacks[1]).getPassword();
		password=new String(p);

		System.err.println("Authenticating " + username
			+ " with the password " + password);
		authed=true;
		return(true);
	}

	/**
	 * Commit the authentication, all is well.
	 */
	public boolean commit() throws LoginException {
		if(authed) {
			principal=new SpyPrincipal(username);
			Set principals=subject.getPrincipals();

			if(!principals.contains(principal)) {
				principals.add(principal);
			}

			committed=true;
		}
		return(committed);
	}

	/**
	 * Abort the authentication process when the LoginContext
	 * authentication fails.
	 */
	public boolean abort() throws LoginException {
		boolean rv=false;

		if(authed) {
			if(committed) {
				// This auth succeeded, but something else failed.
				logout();
			} else {
				authed=false;
				principal=null;
			}
		}

		return(rv);
	}

	/**
	 * Unlogin.
	 */
	public boolean logout() throws LoginException {
		Set principals=subject.getPrincipals();
		principals.remove(principal);
		authed=committed;
		principal=null;
		return(true);
	}

}
