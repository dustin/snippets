// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: SpySecurityException.java,v 1.2 2002/07/10 04:24:55 dustin Exp $

package net.spy.aaa;

import java.io.PrintStream;
import java.io.PrintWriter;

// Yet another copy of Nested*Exception.java :(

/**
 * An Exception that will allow chaining of another Throwable.
 */
public class SpySecurityException extends RuntimeException {

	private Throwable root=null;

	/**
	 * Allow subclasses to get an exception without a message.
	 */
	protected SpySecurityException() {
		super();
	}

	/**
	 * Get an instance of SpySecurityException with a given message.
	 */
	public SpySecurityException(String msg) {
		super(msg);
	}

	/**
	 * Get a SpySecurityException with a given message and root cause
	 * throwable.
	 */
	public SpySecurityException(String msg, Throwable t) {
		super(msg);
		root=t;
	}

	/**
	 * Get the root cause of this problem.
	 */
	public Throwable getRootCause() {
		return(root);
	}

	/**
	 * Set the root cause of this problem.
	 */
	protected void setRootCause(Throwable root) {
		this.root=root;
	}

	/**
	 * String me.
	 */
	public String toString() {
		String rv=null;

		if(root==null) {
			rv=super.toString();
		} else {
			rv=super.toString() + " because of " + root.toString();
		}

		return(rv);
	}

	/**
	 * Print the stack and the root stack (if any).
	 */
	public void printStackTrace(PrintStream s) {
		super.printStackTrace(s);
		if(root!=null) {
			s.println("*** Root Cause Stack:");
			root.printStackTrace(s);
		}
	}

	/**
	 * Print the stack and the root stack (if any).
	 */
	public void printStackTrace(PrintWriter s) {
		super.printStackTrace(s);
		if(root!=null) {
			s.println("*** Root Cause Stack:");
			root.printStackTrace(s);
		}
	}

	/**
	 * Print the stack and the root stack (if any).
	 */
	public void printStackTrace() {
		super.printStackTrace();
		if(root!=null) {
			System.err.println("*** Root Cause Stack:");
			root.printStackTrace();
		}
	}

}
