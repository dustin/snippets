// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: RObject.java,v 1.2 1999/09/14 23:55:11 dustin Exp $

import java.rmi.Remote;
import java.rmi.RemoteException;

import java.util.*;

public interface RObject extends Remote {
    void storeObject(String name, Object o) throws RemoteException;
    Object getObject(String name) throws RemoteException;
}
