// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObjectImpl.java,v 1.6 2000/06/30 05:59:14 dustin Exp $

package net.spy.rmi;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.lang.*;
import java.io.*;

/**
 * Implementation for RObjectServer
 */
public class RObjectImpl extends UnicastRemoteObject implements RObject {

	// Number of hash directory levels.
	public int levels = 256;
	// Base directory for hashing
	public String basedir = "/tmp/rcache";

	public RObjectImpl() throws RemoteException {
		super();
	}

	public RObjectImpl(String basedir) throws RemoteException {
		super();
		this.basedir=basedir;
	}

    public void storeObject(String name, Object o) throws RemoteException {
		int hash, subhash;
		String pathto;
		File f;

		System.out.println("Saving the object: " + name);

		hash=name.hashCode();
		subhash=hash%levels;
		pathto=basedir + "/" + subhash + "/" + hash;

		f=new File(basedir + "/" + subhash);
		if(!f.isDirectory()) {
			System.out.println("Making directories for " + f.getPath());
			f.mkdirs();
		}

		try {
			FileOutputStream ostream = new FileOutputStream(pathto);
			ObjectOutputStream p = new ObjectOutputStream(ostream);
			p.writeObject(o);
			p.flush();
			ostream.close();
		} catch(Exception e) {
			System.err.println("Got an exception:  " + e.getMessage());
			throw new RemoteException(e.getMessage());
		}
	}

    public Object getObject(String name) throws RemoteException {
		int hash, subhash;
		String pathto;

		System.out.println("Giving the object '" + name + "' back...");

		hash=name.hashCode();
		subhash=hash%levels;
		pathto=basedir + "/" + subhash + "/" + hash;

		try {
			Object o;
			FileInputStream istream = new FileInputStream(pathto);
			ObjectInputStream p = new ObjectInputStream(istream);
			o = p.readObject();
			return(o);
		} catch(Exception e) {
			System.err.println("Got an exception:  " + e.getMessage());
		}
		return(null);
	}

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
