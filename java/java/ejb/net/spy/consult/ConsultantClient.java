// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ConsultantClient.java,v 1.2 2000/01/22 11:42:52 dustin Exp $

package net.spy.consult;

import java.util.*;
import java.rmi.*;
import javax.naming.*;

public class ConsultantClient {
	public static void main(String args[]) throws Exception {
		Consultant c=null;
		Context ctx=getInitialContext();
		ConsultantHome h = (ConsultantHome)ctx.lookup("ConsultantHome");

		try {
			c=h.findByPrimaryKey(new ConsultantPK(1));
			System.out.println(c.firstName() + " " + c.lastName());
		} catch(Exception e) {
			System.err.println("Error finding by primary key:  " + e);
		}

		if(args.length>1) {
			c=h.create(args[0], args[1]);
		}
	}

	protected static Context getInitialContext() throws Exception {
		Properties p = new Properties();
		p.put("java.naming.provider.url", "rmi://foo:1099");
		p.put("java.naming.factory.initial",
			"com.sun.jndi.rmi.registry.RegistryContextFactory");
		return(new InitialContext(p));
	}
}
