// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: ImageServer.java,v 1.1 1999/11/24 09:06:48 dustin Exp $

package net.spy;

import java.rmi.Remote; 
import java.rmi.RemoteException; 

import java.util.*;
import net.spy.util.*;

public interface ImageServer extends Remote { 
	public ImageData getImage(int image_id, boolean thumbnail)
		throws RemoteException;
	public void storeImage(int image_id, ImageData image)
		throws RemoteException;
}
