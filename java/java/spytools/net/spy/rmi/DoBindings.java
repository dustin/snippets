// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: DoBindings.java,v 1.2 2000/06/20 07:14:33 dustin Exp $

package net.spy.rmi;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

public class DoBindings {
	public static void main(String args[]) throws Exception {
		if (System.getSecurityManager() == null) {
			System.setSecurityManager(new RMISecurityManager());
		}

		if(args.length<1) {
			System.err.println("RCache path not given.");
			throw new Exception("RCache path not given.");
		}

		if(args.length<2) {
			System.err.println("ImageServer config path not given.");
			throw new Exception("ImageServer config path not given.");
		}

		try {
			RObjectImpl obj1 = new RObjectImpl(args[0]);
			Naming.rebind("RObjectServer", obj1);
			System.out.println("RObjectServer bound in registry");
			ImageServerImpl obj2 = new ImageServerImpl(args[1]);
			Naming.rebind("ImageServer", obj2);
			System.out.println("ImageServer bound in registry");
		} catch(Exception e) {
			System.err.println("Binding error:  " + e);
			e.printStackTrace();
		}
	}
}
