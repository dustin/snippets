// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpySecurityManager.java,v 1.2 2002/07/10 04:24:57 dustin Exp $

package net.spy.aaa;

import java.security.Permission;
import java.security.PermissionCollection;

import java.util.Enumeration;

import net.spy.util.TimeStampedHash;

/**
 * A flexible SecurityManager type thing for use within applications.
 */
public class SpySecurityManager extends Object {

	private static final String MARKER=null;

	private static TimeStampedHash managers=null;

	private Authenticator auther=null;
	private TimeStampedHash tokens=null;

	private SpyPolicy policy=null;

	/**
	 * Get an instance of SpySecurityManager.
	 */
	public SpySecurityManager() {
		super();
		if(managers==null) {
			initManagers();
		}

		tokens=new TimeStampedHash();
	}

	private static synchronized void initManagers() {
		if(managers==null) {
			managers=new TimeStampedHash();
		}
	}

	// Static stuff

	/**
	 * Get a security manager by name.
	 */
	public static SpySecurityManager getSecurityManager(String name) {
		// XXX Insert security check here
		SpySecurityManager ssm=null;
		synchronized(managers) {
			ssm=(SpySecurityManager)managers.get(name);
		}
		return(ssm);
	}

	/**
	 * Set a security manager by name.
	 */
	public static void setSecurityManager(String name, SpySecurityManager ssm){
		// XXX Insert security check here
		synchronized(managers) {
			managers.put(name, ssm);
		}
	}

	// end static stuff

	/**
	 * Set the authenticator for this manager.
	 */
	public void setAuthenticator(Authenticator a) {
		if(auther!=null) {
			throw new SpySecurityException(
				"The authenticator may not be changed.");
		}
		this.auther=a;
	}

	private void checkToken(Token tok) {
		if(tok!=null) {
			synchronized(tokens) {
				if(!tokens.containsKey(tok)) {
					throw new SpySecurityException("Fraudulent token: " + tok);
				}
			}
		}
	}

	/**
	 * Get the policy.
	 */
	protected SpyPolicy getPolicy() {
		return(policy);
	}

	/**
	 * Find out of this user has this permission.
	 */
	public void checkPermission(Token tok, Permission perm) {
		// First, verify the token.
		checkToken(tok);

		// Get the policy.
		SpyPolicy sp=getPolicy();
		PermissionCollection pc=sp.getPermissions(tok);

		boolean ok=false;
		for(Enumeration e=pc.elements(); ok==false && e.hasMoreElements();) {
			Permission p=(Permission)e.nextElement();
			// True if the objects are equal or the pimp lies.
			if(p.equals(perm) || p.implies(perm)) {
				ok=true;
			}
		}

		// If we don't have permission, throw an exception.
		if(!ok) {
			throw new SpySecurityPermissionException(tok,perm);
		}
	}

	/**
	 * Attempt to authenticate an Authable with the given password.
	 */
	public Token authenticate(Authable a, String password)
		throws AuthException {

		// Get the token
		Token t=auther.authUser(a, password);
		// Save a copy in our stash
		synchronized(tokens) {
			tokens.put(t, MARKER);
		}
		return(t);
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
	}

}
