package net.spy.netinfo;

import java.lang.*;
import java.util.*;
import java.sql.*;

public class Getit extends Thread {
	Stack stuff=null;
	Connection netinfodb;

	public Getit(Stack v, Connection db) {
		super();
		stuff=v;
		netinfodb=db;
	}

	public boolean getone() {
		boolean ret=false;
		try {
			Service s = (Service)stuff.pop();
			Info i = new Info(s.host, s.port);
			String summary=i.summary();
			System.out.println(s.host + ":" + s.port + ":  " + summary);
				ret=true;
			// Store that shit, homey
			store(s.host, s.port, summary);
		} catch(Exception e) {
		}
		return(ret);
	}

	protected synchronized void store(String host, int port, String svc)
		throws Exception {
		Statement st = netinfodb.createStatement();
		st.executeUpdate("delete from services where host='" + host
			+ "' and port = " + port);
		st.executeUpdate("insert into services values('" + host
			+ "', " + port + ", '" + svc + "')");
	}

	public void run() {
		while(getone());
	}
}
