// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObject.java,v 1.3 2000/06/30 05:59:13 dustin Exp $

package net.spy.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

import java.util.*;

/**
 * RObject is an RMI service that allows you to access a hash table stored
 * on a remote machine's disk.
 */

public interface RObject extends Remote {
	/**
	 * Store an Object in the object server.
	 *
	 * @param name Key under which the object will be stored
	 * @param o    Object to store
	 *
	 * @throws RemoteException when something breaks
	 */
    void storeObject(String name, Object o) throws RemoteException;

	/**
	 * Fetch an object from the object server.
	 *
	 * @param name Name of the object to fetch.
	 *
	 * @return the object
	 *
	 * @throws RemoteException when something breaks
	 */
    Object getObject(String name) throws RemoteException;

	/**
	 * Make sure the server is available and functioning.
	 *
	 * @return true if the server is awake.
	 *
	 * @throws RemoteException when something breaks
	 */
    boolean ping() throws RemoteException;
}
