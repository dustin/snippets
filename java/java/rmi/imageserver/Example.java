// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: Example.java,v 1.1 1999/11/24 09:06:47 dustin Exp $

import java.util.*;
import java.lang.*;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.util.*;

import net.spy.*;
import net.spy.util.*;

public class Example {
	public static void main(String args[]) {
		try {
			ImageServer i = new ImageServerImpl();
			i = (ImageServer)Naming.lookup("//localhost/ImageServer");
			ImageData image = i.getImage(505, false);
		} catch(Exception e) {
			System.out.println("Exception:  " + e);
		}
		System.exit(0);
	}
}
