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

    public void storeHash(Hashtable h) throws RemoteException {
		thehash=h;
		System.out.println("Saving a hash...");
	}

    public Hashtable getHash() throws RemoteException {
		System.out.println("Giving the hash back...");
		return(thehash);
	}

	public static void main(String args[]) {

		thehash = null;

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
