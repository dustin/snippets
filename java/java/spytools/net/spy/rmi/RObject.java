// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObject.java,v 1.2 2000/01/25 06:49:54 dustin Exp $

package net.spy.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

import java.util.*;

public interface RObject extends Remote {
    void storeObject(String name, Object o) throws RemoteException;
    Object getObject(String name) throws RemoteException;
    boolean ping() throws RemoteException;
}
