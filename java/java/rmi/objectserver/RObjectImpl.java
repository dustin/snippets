// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObjectImpl.java,v 1.2 1999/09/14 23:55:15 dustin Exp $

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.lang.*;

public class RObjectImpl extends UnicastRemoteObject implements RObject {

	static Hashtable thehash;

	public RObjectImpl() throws RemoteException {
		super();
	}

    public void storeObject(String name, Object o) throws RemoteException {
		thehash.put(name, o);
		System.out.println("Saving the object: " + name);
	}

    public Object getObject(String name) throws RemoteException {
		System.out.println("Giving the object '" + name + "' back...");
		return(thehash.get(name));
	}

	public static void main(String args[]) {

		thehash = new Hashtable();

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
