// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: DSBean.java,v 1.1 2000/11/06 07:59:27 dustin Exp $

package net.spy.dsservlet;

import java.sql.*;
import java.util.*;
import net.spy.*;

public class DSBean extends Object {

	protected String username="guest";

	public DSBean() {
		super();
	}

	public void setUsername(String username) {
		this.username=username;
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
