// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: ImageServerImpl.java,v 1.1 1999/11/24 09:06:49 dustin Exp $

package net.spy;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.UnicastRemoteObject;

import java.util.*;
import java.lang.*;
import java.io.*;

import net.spy.*;
import net.spy.util.*;

public class ImageServerImpl extends UnicastRemoteObject
	implements ImageServer {

	public ImageServerImpl() throws RemoteException {
		super();
	}

	public ImageData getImage(int image_id, boolean thumbnail)
		throws RemoteException {
		throw new RemoteException("Not implemented.");
	}

	public void storeImage(int image_id, ImageData image)
		throws RemoteException {
		throw new RemoteException("Not implemented.");
	}

	public static void main(String args[]) {

		// Create and install a security manager
		if (System.getSecurityManager() == null) {
			System.setSecurityManager(new RMISecurityManager());
		}
		try {
			ImageServerImpl obj = new ImageServerImpl();
			// Bind this object instance to the name "RobjectServer"
			Naming.rebind("ImageServer", obj);
			System.out.println("ImageServer bound in registry");
		} catch (Exception e) {
			System.out.println("ImageServer err: " + e.getMessage());
			e.printStackTrace();
		}
	}
}
