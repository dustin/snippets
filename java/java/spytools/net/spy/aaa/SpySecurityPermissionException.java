// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpySecurityPermissionException.java,v 1.2 2002/08/21 00:52:58 dustin Exp $

package net.spy.aaa;

import java.security.Permission;

/**
 * Security Exceptions thrown from Spy stuff.
 */
public class SpySecurityPermissionException extends SpySecurityException {

	private Token tok=null;
	private Permission perm=null;

	/**
	 * Get a SpySecurityPermissionException.
	 */
	public SpySecurityPermissionException(Token t, Permission perm) {
		super();
		this.tok=t;
		this.perm=perm;
	}

	/**
	 * Get a SpySecurityPermissionException.
	 */
	public SpySecurityPermissionException(Token t, Permission perm,
		Throwable root) {

		this(t, perm);
		setRootCause(root);
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer(64);
		String username="unauthenticated user";
		if(tok!=null) {
			username=tok.getUsername();
		}
		sb.append(username);
		sb.append(" doesn't have ");
		sb.append(perm);
		return(sb.toString());
	}

}
