// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyPolicy.java,v 1.2 2002/07/10 04:24:54 dustin Exp $

package net.spy.aaa;

import java.security.Permission;
import java.security.PermissionCollection;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

/**
 * A Policy for use in SpySecurityManager.
 */
public class SpyPolicy extends Object {

	// Permissions for non-authenticated (any) users
	private Vector nullPermissions=null;
	// Permissions for authenticated users
	private Vector authPermissions=null;
	// Permissions for specific authenticated users
	private Hashtable userPermissions=null;

	/**
	 * Get an instance of SpyPolicy.
	 */
	public SpyPolicy() {
		super();
		nullPermissions=new Vector();
		authPermissions=new Vector();
		userPermissions=new Hashtable();
	}

	/**
	 * String me.
	 */
	public String toString() {
		return("SPY Security Policy");
	}

	/**
	 * Get the permissions for unauthenticated users.
	 */
	protected Enumeration getNullPermissions() {
		return(nullPermissions.elements());
	}

	/**
	 * Get the permissions for authenticated users.
	 */
	protected Enumeration getAuthPermissions() {
		return(authPermissions.elements());
	}

	/**
	 * Get the user-specific permissions based on a token.
	 */
	protected Enumeration getUserPermissions(Token tok) {
		Vector up=(Vector)userPermissions.get(tok.getUsername());
		if(up==null) {
			up=new Vector();
		}
		return(up.elements());
	}

	/**
	 * Get a PermissionCollection for a Token.
	 */
	public PermissionCollection getPermissions(Token tok) {
		SpyPermissionCollection rv=new SpyPermissionCollection();

		// Get the null permissions (permissions that apply to everyone)
		for(Enumeration e=getNullPermissions(); e.hasMoreElements();) {
			rv.add((Permission)e.nextElement());
		}

		// If a token is provided, get the permissions that token carries.
		if(tok!=null) {
			// Add auth user stuff
			for(Enumeration e=getAuthPermissions(); e.hasMoreElements();) {
				rv.add((Permission)e.nextElement());
			}

			// Add user-specific stuff.
			for(Enumeration e=getUserPermissions(tok); e.hasMoreElements();) {
				rv.add((Permission)e.nextElement());
			}
		}

		return(rv);
	}

	/**
	 * Clear the policy cache.
	 */
	public void refresh() {
	}

}
