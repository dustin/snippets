// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: Example.java,v 1.2 1999/11/25 09:37:00 dustin Exp $

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
			ImageData image = i.getImage(600, true);
			dumpit(image);
		} catch(Exception e) {
			System.out.println("Exception:  " + e);
		}
		System.exit(0);
	}

	public static void dumpit(ImageData d) throws Exception {
		Vector v=d.image_data;
		int i;
		for(i=0; i<v.size(); i++) {
			System.out.write( (byte[])v.elementAt(i));
		}
	}
}
