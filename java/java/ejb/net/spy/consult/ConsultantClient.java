// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ConsultantClient.java,v 1.1 2000/01/22 10:25:18 dustin Exp $

package net.spy.consult;

import java.util.*;
import java.rmi.*;
import javax.naming.*;

public class ConsultantClient {
	public static void main(String args[]) throws Exception {
		Context ctx=getInitialContext();
		ConsultantHome h = (ConsultantHome)ctx.lookup("ConsultantHome");

		Consultant c=h.findByPrimaryKey(new ConsultantPK(1));

		System.out.println(c.firstName() + " " + c.lastName());
	}

	protected static Context getInitialContext() throws Exception {
		Properties p = new Properties();
		p.put("java.naming.provider.url", "rmi://foo:1099");
		p.put("java.naming.factory.initial",
			"com.sun.jndi.rmi.registry.RegistryContextFactory");
		return(new InitialContext(p));
	}
}
