// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObjectImpl.java,v 1.9 2002/02/21 01:32:32 dustin Exp $

package net.spy.rmi;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.security.*;
import java.io.*;

/**
 * Implementation for RObjectServer
 */
public class RObjectImpl extends UnicastRemoteObject implements RObject {

	// Number of hash directory levels.
	public static final int levels = 256;
	// Base directory for hashing
	public String basedir = "/tmp/rcache";

	public RObjectImpl() throws RemoteException {
		super();
	}

	public RObjectImpl(String basedir) throws RemoteException {
		super();
		this.basedir=basedir;
	}

	public String getPath(String key) {
		MessageDigest md=null;
		try {
			md=MessageDigest.getInstance("SHA");
		} catch(NoSuchAlgorithmException e) {
			throw new Error("There's no SHA?");
		}
		md.update(key.getBytes());

		String hashed=net.spy.SpyUtil.byteAToHexString(md.digest());

		String base=basedir+"/"+hashed.substring(0,2);
		String path=basedir+"/"+hashed.substring(0,2)+"/"+hashed;

		File f=new File(base);
		if(!f.isDirectory()) {
			f.mkdirs();
		}

		return(path);
	}

    public void storeObject(String name, Object o) throws RemoteException {
		String pathto=getPath(name);

		try {
			FileOutputStream ostream = new FileOutputStream(pathto);
			ObjectOutputStream p = new ObjectOutputStream(ostream);
			p.writeObject(o);
			p.flush();
			ostream.close();
		} catch(Exception e) {
			System.err.println("RObject exception while storing object:  "
				+ e);
			throw new RemoteException(e.getMessage());
		}
	}

    public Object getObject(String name) throws RemoteException {
		String pathto=getPath(name);

		try {
			Object o;
			FileInputStream istream = new FileInputStream(pathto);
			ObjectInputStream p = new ObjectInputStream(istream);
			o = p.readObject();
			return(o);
		} catch(Exception e) {
			System.err.println("RObject exception while getting object:  "
				+ e);
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
