package net.spy.testsb;

import java.rmi.RemoteException;
import javax.ejb.CreateException;
import javax.ejb.EJBHome;

public interface TestSBHome extends EJBHome {
    TestSB create(String user)  throws CreateException, RemoteException;
}
