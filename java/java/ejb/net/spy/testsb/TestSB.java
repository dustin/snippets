package net.spy.testsb;

import java.rmi.RemoteException;
import javax.ejb.EJBObject;

public interface TestSB extends EJBObject {
    public void buy (int Shares)  throws RemoteException;
    public int  read ()           throws RemoteException;
}
