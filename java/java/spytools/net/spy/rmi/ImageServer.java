// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: ImageServer.java,v 1.2 2000/04/17 01:31:03 dustin Exp $

package net.spy.rmi;

import java.rmi.Remote;
import java.rmi.RemoteException;

import java.util.*;
import net.spy.util.*;

public interface ImageServer extends Remote {
	public ImageData getImage(int image_id, boolean thumbnail)
		throws RemoteException;
	public void storeImage(int image_id, ImageData image)
		throws RemoteException;
	public boolean ping() throws RemoteException;
}
