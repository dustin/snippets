// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyPermissionCollection.java,v 1.1 2001/07/11 09:30:52 dustin Exp $

package net.spy.aaa;

import java.security.*;
import java.util.*;

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
