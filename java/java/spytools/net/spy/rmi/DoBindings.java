// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: DoBindings.java,v 1.1 1999/11/26 01:00:12 dustin Exp $

package net.spy.rmi;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

public class DoBindings {
	public static void main(String args[]) {
		if (System.getSecurityManager() == null) {
			System.setSecurityManager(new RMISecurityManager());
		}

		try {
			RObjectImpl obj1 = new RObjectImpl();
			Naming.rebind("RObjectServer", obj1);
			System.out.println("RObjectServer bound in registry");
			ImageServerImpl obj2 = new ImageServerImpl();
			Naming.rebind("ImageServer", obj2);
			System.out.println("ImageServer bound in registry");
		} catch(Exception e) {
			System.out.println("Binding error:  " + e);
			e.printStackTrace();
		}
	}
}
