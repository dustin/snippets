// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyPrincipal.java,v 1.1 2001/07/10 09:22:43 dustin Exp $

package net.spy.aaa;

import java.security.*;

/**
 * A Simple Principal.
 */
public class SpyPrincipal extends Object implements Principal {

	private String username=null;

	/**
	 * Get an instance of SpyPrincipal.
	 */
	public SpyPrincipal(String username) {
		super();
		this.username=username;
		if(this.username==null) {
			throw new NullPointerException("You can't have a null principal");
		}
	}

	/**
	 * String me.
	 */
	public String toString() {
		return(getName());
	}

	/**
	 * Get the name of this Principal.
	 */
	public String getName() {
		return(username);
	}

	/**
	 * Return true of this object and that object represent the same thing.
	 */
	public boolean equals(Object o) {
		boolean rv=false;
		System.out.println("Testing " + this + " and " + o + " for equality");
		if(o!=null) {
			if(this==o) {
				rv=true;
			} else {
				if(o instanceof SpyPrincipal) {
					SpyPrincipal sp=(SpyPrincipal)o;
					rv=getName().equals(sp.getName());
				}
			}
		}

		return(rv);
	}

	/**
	 * Get a unique identifier for this thing.
	 */
	public int hashCode() {
		return(username.hashCode());
	}

}
