// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyPermissionCollection.java,v 1.2 2002/07/10 04:24:53 dustin Exp $

package net.spy.aaa;

import java.security.Permission;
import java.security.PermissionCollection;

import java.util.Enumeration;
import java.util.Vector;

/**
 * A collection of permissions.
 */
public class SpyPermissionCollection extends PermissionCollection {

	// The actual permissions are stored here.
	private Vector permissions=null;

	/**
	 * Get an instance of SpyPermissionCollection.
	 */
	public SpyPermissionCollection() {
		super();
		permissions=new Vector();
	}

	/**
	 * Add a new permission to the collection.
	 */
	public void add(Permission permission) {
		if(isReadOnly()) {
			throw new SecurityException("Read Only");
		}
		permissions.add(permission);
	}

	/**
	 * Returns true if this collection implies the passed permission (which
	 * it doesn't).
	 */
	public boolean implies(Permission permission) {
		return(false);
	}

	/**
	 * Get the Permission objects contained herein.
	 */
	public Enumeration elements() {
		return(permissions.elements());
	}

}
