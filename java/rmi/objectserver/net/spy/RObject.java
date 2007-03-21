// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObject.java,v 1.1 1999/10/20 03:02:04 dustin Exp $

package net.spy;

import java.rmi.Remote; 
import java.rmi.RemoteException; 

import java.util.*;

public interface RObject extends Remote { 
    void storeObject(String name, Object o) throws RemoteException; 
    Object getObject(String name) throws RemoteException; 
}
