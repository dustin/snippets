// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: AAATest.java,v 1.1 2001/07/10 09:22:42 dustin Exp $

package net.spy.aaa;

import java.security.*;
import java.security.cert.Certificate;
import java.util.*;
import java.net.*;
import java.io.*;

import javax.security.*;
import javax.security.auth.*;
import javax.security.auth.login.*;
import javax.security.auth.Policy;

/**
 * Test for Simple AAA Services.
 *
 * <p>
 *
 * java -Djava.security.policy=/path -Djava.security.auth.policy=/path
 * -Djava.security.auth.login.config=/path -Djava.security.manager
 */
public class AAATest extends Object {

	/**
	 * Get an instance of AAATest.
	 */
	public AAATest() {
		super();
	}

	// This is an operation that will throw an exception if there's no
	// permission.
	private static void privedOperation() {
		File f = new File("/etc/passwd");
		boolean e=f.exists(); // This will throw a security exception
		System.out.println("Did something bad (" + f + ":" + e + ")" );
		new Exception().printStackTrace();
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		LoginContext lc=new LoginContext("Test",
			new SimpleCallbackHandler(args[0], args[1]));
		lc.login();

		Subject.doAsPrivileged(lc.getSubject(), new PrivilegedAction() {
				public Object run() {
					privedOperation();
					return(null);
				}
			}, null);

		Policy p=Policy.getPolicy();
		Certificate certs[]=new Certificate[0];
		PermissionCollection pc=p.getPermissions(lc.getSubject(),
			new CodeSource(new URL("file:/Users/dustin/prog/java/spytools/"),
			certs));
		System.out.println("Permissions:");
		for(Enumeration e=pc.elements(); e.hasMoreElements(); ) {
			System.out.println("\t" + e.nextElement());
		}
	}

}
