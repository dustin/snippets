// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
//$Id: SafetySecurityManager.java,v 1.1 2001/01/29 06:45:40 dustin Exp $

package net.spy.safety;

import java.io.*;
import java.net.*;

/**
 * A simple security manager to prevent bad things from happening.
 */
public class SafetySecurityManager extends SecurityManager {

	public SafetySecurityManager() {
		super();
	}

	protected String getStackString() {
		Class stack[]=getClassContext();
		StringBuffer sb=new StringBuffer();
		for(int i=0; i<stack.length; i++) {
			sb.append(stack[i].getName());
			sb.append(" ");
		}

		return(sb.toString().trim());
	}

	/**
	 * Deny access to System.exit();
	 */
	public void checkExit(int status) {
		throw new SecurityException("No way you can exit from "
			+ getStackString());
	}

	public void checkAccept(String s, int i) throws SecurityException {
	}

	public void checkAccess(Thread t) throws SecurityException {
	}

	public void checkAccess(ThreadGroup t) throws SecurityException {
	}

	public void checkAwtEventQueueAccess() throws SecurityException {
	}

	public void checkConnect(String s, int i) throws SecurityException {
	}

	public void checkConnect(String s, int i, Object o)
		throws SecurityException {
	}

	public void checkCreateClassLoader() throws SecurityException {
	}

	public void checkDelete(String s) throws SecurityException {
	}

	public void checkExec(String s) throws SecurityException {
		throw new SecurityException(
			"Not allowed to exec " + s + " from " + getStackString());
	}

	public void checkLink(String s) throws SecurityException {
	}

	public void checkListen(int i) throws SecurityException {
	}

	public void checkMemberAccess(Class c, int i) throws SecurityException {
	}

	public void checkMulticast(InetAddress addr)
		throws SecurityException {
	}

	public void checkMulticast(InetAddress addr, byte b)
		throws SecurityException {
	}

	public void checkPackageAccess(String s) throws SecurityException {
	}

	public void checkPackageDefinition(String s) throws SecurityException {
	}

	public void checkPrintJobAccess() throws SecurityException {
		throw new SecurityException(
			"Not allowed to print from " + getStackString());
	}

	public void checkPropertiesAccess() throws SecurityException {
	}

	public void checkPropertyAccess(String s) throws SecurityException {
	}

	public void checkRead(FileDescriptor fd) throws SecurityException {
	}

	public void checkRead(String s) throws SecurityException {
	}

	public void checkRead(String s, Object o) throws SecurityException {
	}

	public void checkSecurityAccess(String s) throws SecurityException {
	}

	public void checkSetFactory() throws SecurityException {
	}

	public void checkSystemClipboardAccess() throws SecurityException {
	}

	public boolean checkTopLevelWindow(Object o) throws SecurityException {
		return(false);
	}

	public void checkWrite(FileDescriptor fd) throws SecurityException {
	}

	public void checkWrite(String s) throws SecurityException {
	}

}
