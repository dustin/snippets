// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObjectImpl.java,v 1.10 2002/06/17 02:46:49 dustin Exp $

package net.spy.rmi;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.security.*;
import java.io.*;

import net.spy.cache.*;

/**
 * Implementation for RObjectServer
 */
public class RObjectImpl extends UnicastRemoteObject implements RObject {

	private static final String DEFAULT_DIR="/tmp/rcache";

	private DiskCache diskCache=null;

	/**
	 * Get an RObjectImpl with the default directory.
	 */
	public RObjectImpl() throws RemoteException {
		this(DEFAULT_DIR);
	}

	/**
	 * Get an RObjectImpl with the given directory.
	 */
	public RObjectImpl(String basedir) throws RemoteException {
		super();
		diskCache=new DiskCache(basedir);
	}

	/**
	 * @see RObject
	 */
    public void storeObject(String name, Object o) throws RemoteException {
		try {
			diskCache.storeObject(name, o);
		} catch(IOException e) {
			throw new RemoteException("Error storing object", e);
		}
	}

	/**
	 * @see RObject
	 */
    public Object getObject(String name) throws RemoteException {
		return(diskCache.getObject(name));
	}

	/**
	 * @see RObject
	 */
	public boolean ping() throws RemoteException {
		return(true);
	}

	/**
	 * main can be invoked to run an RObjectServer.  It takes as an
	 * argument, the path where it will be storing its objects.
	 *
	 * @param args Uh, yeah, the arguments
	 *
	 * @throws Exception if anything blows up
	 */
	public static void main(String args[]) throws Exception {
		if(args.length < 1) {
			System.err.println("RCache path not given.");
			throw new Exception("RCache path not given.");
		}
		RObjectImpl obj1 = new RObjectImpl(args[0]);
		Naming.rebind("RObjectServer", obj1);
		System.err.println("RObjectServer bound in registry");
	}
}
