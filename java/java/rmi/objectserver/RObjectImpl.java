// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObjectImpl.java,v 1.3 1999/09/23 04:50:06 dustin Exp $

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.lang.*;
import java.io.*;

public class RObjectImpl extends UnicastRemoteObject implements RObject {

	public RObjectImpl() throws RemoteException {
		super();
	}

    public void storeObject(String name, Object o) throws RemoteException {
		System.out.println("Saving the object: " + name);

		try {
			FileOutputStream ostream = new FileOutputStream("/tmp/o/" + name);
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
		System.out.println("Giving the object '" + name + "' back...");

		try {
			Object o;
			FileInputStream istream = new FileInputStream("/tmp/o/" + name);
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
