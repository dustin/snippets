// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Consultant.java,v 1.1 2000/01/22 09:45:15 dustin Exp $

package net.spy.consult;

import javax.ejb.*;
import java.rmi.RemoteException;

public interface Consultant extends EJBObject {

	// Name
	public String firstName() throws RemoteException;
	public void setFirstName(String to) throws RemoteException;
	public String lastName() throws RemoteException;
	public void setLastName(String to) throws RemoteException;

	// Social Security Number
	public String SSN() throws RemoteException;
	public void setSSN(String to) throws RemoteException;

	// Address info
	public Address address() throws RemoteException;
	public void setAddress(Address to) throws RemoteException;
}
