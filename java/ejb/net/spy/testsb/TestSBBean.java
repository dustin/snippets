package net.spy.testsb;

import java.io.*;
import java.rmi.RemoteException;
import javax.ejb.EJBObject;
import javax.ejb.SessionBean;
import javax.ejb.SessionContext;
import javax.ejb.SessionSynchronization;

public class TestSBBean implements SessionBean, SessionSynchronization {

    protected int total = 0;	// actual state of the bean
    protected int newtotal = 0;	// value inside Tx, not yet committed.
    protected String clientUser = null;
    protected SessionContext sessionContext = null;

    public void  ejbCreate(String user) throws RemoteException {

	total = 0;
	clientUser = user;
    }

    public void ejbActivate() throws RemoteException {
	// Nothing to do for this simple example
    }


    public void ejbPassivate() throws RemoteException {
	// Nothing to do for this simple example
    }


    public void ejbRemove() throws RemoteException {
	// Nothing to do for this simple example
    }


    public void setSessionContext(SessionContext sessionContext) throws RemoteException {
	this.sessionContext = sessionContext;
    }

    public void afterBegin() throws RemoteException {
	newtotal = total;
    }

    public void beforeCompletion() throws RemoteException {
	// Nothing to do for this simple example
    }

    public void afterCompletion(boolean committed) throws RemoteException {
	if (committed) {
	    total = newtotal;
	}
    }


    /*==================== TestSB implementation =======================*/

    /**
     * Business method implementation.
     * @param s  nb of shares to be bought
     */
    public void buy(int s) {
	newtotal = newtotal + s;
	return;
    }

    /**
     * Business method implementation.
     * @return  the nb of shares bought
     */
    public int read() {
	return newtotal;
    }
}
