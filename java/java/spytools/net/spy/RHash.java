// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RHash.java,v 1.4 2001/02/07 06:31:01 dustin Exp $

package net.spy;

import java.rmi.Naming;
import java.rmi.RemoteException;

import java.util.*;

import net.spy.rmi.*;

/**
 * Abstract client for Remote Hash service.
 */

public class RHash {

	private String rhashserver=null;
	private RObject obj=null;

	/**
	 * Constructor!
	 *
	 * @param server RMI URL to ObjectServer, for example:
	 * rmi://rmiregistoryserverthing/ObjectServer
	 *
	 * @exception Exception An exception is thrown if an RMI connection
	 * cannot be established.
	 */
	public RHash(String server) throws Exception {
		rhashserver = server;
		obj = getobject();
	}

	/**
	 * gets an object from the remote object server.
	 *
	 * @param name name of the object to fetch.
	 *
	 * @return the object from the remote object server.
	 */
	public Object get(String name) {
		Object o;
		try {
			o = obj.getObject(name);
		} catch(Exception e) {
			e.printStackTrace();
			o = null;
		}
		return(o);
	}

	/**
	 * stores an object in the remote object server.
	 *
	 * @param name key under which to store the object
	 * @param o object to store.
	 */
	public void put(String name, Object o) {
		try {
			obj.storeObject(name, o);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Verify we still have a connection to the RMI server.
	 *
	 * @return true if connected
	 */
	public boolean connected() {
		boolean ret=false;
		try {
			ret=obj.ping();
		} catch(Exception e) {
			// Doesn't matter
		}
		return(ret);
	}

	protected void finalize() throws Throwable {
		obj = null;
		super.finalize();
	}

	private RObject getobject() throws Exception {
		try {
			RObject o = (RObject)Naming.lookup(rhashserver);
			return(o);
		} catch(Exception e) {
			throw new Exception("Error getting " + rhashserver
				+ " service:  " + e);
		}
	}

	public static void main(String args[]) throws Exception {
		System.out.println("Connecting to local ObjectServer.");
		RHash rh=new RHash("//localhost/RObjectServer");
		System.out.println("Storing something.");
		rh.put("TestString", "Test");
		System.out.println("Waiting ten seconds.");
		Thread.sleep(10000);
		System.out.println("Re-opening ObjectServer connection.");
		rh=new RHash("//localhost/RObjectServer");
		System.out.println("Fetching something.");
		String blah=(String)rh.get("TestString");
		System.out.println("Result:  " + blah);
	}
}
