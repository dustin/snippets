// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObject.java,v 1.1 1999/11/26 01:00:14 dustin Exp $

package net.spy.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

import java.util.*;

public interface RObject extends Remote {
    void storeObject(String name, Object o) throws RemoteException;
    Object getObject(String name) throws RemoteException;
}
