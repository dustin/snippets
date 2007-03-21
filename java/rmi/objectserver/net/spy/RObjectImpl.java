// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObjectImpl.java,v 1.1 1999/10/20 03:02:04 dustin Exp $

package net.spy;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.lang.*;
import java.io.*;

public class RObjectImpl extends UnicastRemoteObject implements RObject {

	// Number of hash directory levels.
	static int levels = 256;
	// Base directory for hashing
	static String basedir = "/tmp/o";

	public RObjectImpl() throws RemoteException {
		super();
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

	public static void main(String args[]) { 

		// Create and install a security manager 
		if (System.getSecurityManager() == null) { 
			System.setSecurityManager(new RMISecurityManager()); 
		} 
		try { 
			RObjectImpl obj = new RObjectImpl(); 
			// Bind this object instance to the name "RobjectServer" 
			Naming.rebind("RObjectServer", obj); 
			System.out.println("RObjectServer bound in registry"); 
		} catch (Exception e) { 
			System.out.println("RObjectServer err: " + e.getMessage()); 
			e.printStackTrace(); 
		} 
	} 
}
