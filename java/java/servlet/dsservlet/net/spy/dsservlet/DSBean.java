// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: DSBean.java,v 1.3 2003/09/08 01:33:53 dustin Exp $

package net.spy.dsservlet;

import java.sql.*;
import java.util.*;
import net.spy.*;

public class DSBean extends Object {

	protected String username="guest";
	protected Hashtable users=null;

	public DSBean() {
		super();
		initUsers();
	}

	public void setUsername(String username) throws Exception {
		// Make sure the user exists
		if(!users.containsKey(username)) {
			throw new Exception("Authentication error");
		}
		this.username=username;
	}

	public String getUsername() {
		return(username);
	}

	// Currently does nothing, maybe someday it'll do something
	public void setPassword(String password) {
	}

	protected void initUsers() {
		users=new Hashtable();
		users.put("dustin", "blah");
		users.put("jennifer", "blah");
		users.put("noelani", "blah");
		users.put("kim", "blah");
	}

	public Enumeration listAll() throws Exception {
		SpyDB db=new SpyDB(
			new SpyConfig("/afs/spy.net/misc/web/etc/misc.conf"));

		// Only me
		if(!username.equals("dustin")) {
			throw new Exception("Access denied");
		}

		PreparedStatement pst=db.prepareStatement(
			"select * from show_distribution\n"
			+ "  order by submitted");
		ResultSet rs=pst.executeQuery();
		Vector v=new Vector();
		while(rs.next()) {
			v.addElement(new Show(rs));
		}
		return(v.elements());
	}

	public Enumeration listUnseen() throws Exception {
		SpyDB db=new SpyDB(
			new SpyConfig("/afs/spy.net/misc/web/etc/misc.conf"));
		PreparedStatement pst=db.prepareStatement(
			"select * from show_distribution\n"
			+ "  where submitted_to=? and completed is null order by submitted"
			);
		pst.setString(1, username);
		ResultSet rs=pst.executeQuery();
		Vector v=new Vector();
		while(rs.next()) {
			v.addElement(new Show(rs));
		}
		return(v.elements());
	}

	public Enumeration list() throws Exception {
		SpyDB db=new SpyDB(
			new SpyConfig("/afs/spy.net/misc/web/etc/misc.conf"));
		PreparedStatement pst=db.prepareStatement(
			"select * from show_distribution\n"
			+ "  where submitted_to=? order by submitted"
			);
		pst.setString(1, username);
		ResultSet rs=pst.executeQuery();
		Vector v=new Vector();
		while(rs.next()) {
			v.addElement(new Show(rs));
		}
		return(v.elements());
	}

	public static void main(String args[]) throws Exception {
		DSBean dsb=new DSBean();
		dsb.setUsername(args[0]);

		System.out.println("Unseen:");
		for(Enumeration e=dsb.listUnseen(); e.hasMoreElements(); ) {
			System.out.println(e.nextElement().toString());
		}
		System.out.println("All:");
		for(Enumeration e=dsb.list(); e.hasMoreElements(); ) {
			System.out.println(e.nextElement().toString());
		}
	}
}
