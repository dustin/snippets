// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Show.java,v 1.2 2000/11/06 08:42:50 dustin Exp $

package net.spy.dsservlet;

import java.sql.*;
import java.io.*;

import net.spy.*;

public class Show extends Object {
	protected String show_id=null;
	protected java.util.Date submitted=null;
	protected String submitted_to=null;
	protected java.util.Date completed=null;

	protected final String filePath="/afs/spy.net/home/dustin/public_html/ds/";

	public Show(ResultSet rs) throws Exception {
		super();
		initFromResultSet(rs);
	}

	public Show(String show_id, String username) throws Exception {
		super();

		SpyDB db=new SpyDB(
			new SpyConfig("/afs/spy.net/misc/web/etc/misc.conf"));
		PreparedStatement pst=db.prepareStatement(
			"select * from show_distribution\n"
			+ " where show_id = ? and submitted_to = ?"
			);
		pst.setString(1, show_id);
		pst.setString(2, username);
		ResultSet rs=pst.executeQuery();
		rs.next();
		initFromResultSet(rs);
	}

	public void markSent() throws Exception {
		SpyDB db=new SpyDB(
			new SpyConfig("/afs/spy.net/misc/web/etc/misc.conf"));
		PreparedStatement pst=db.prepareStatement(
			"update show_distribution set completed = now()\n"
			+ " where show_id = ? and submitted_to = ?"
			);
		pst.setString(1, show_id);
		pst.setString(2, submitted_to);
		pst.executeUpdate();
	}

	public File getFile() {
		return(new File(filePath + show_id));
	}

	protected void initFromResultSet(ResultSet rs) throws Exception {
		show_id=rs.getString("show_id");
		submitted=rs.getDate("submitted");
		submitted_to=rs.getString("submitted_to");
		completed=rs.getTimestamp("completed");
	}

	public String toString() {
		String comp=null;
		if(completed==null) {
			comp="no";
		} else {
			comp=completed.toString();
		}

		return(submitted_to + " " + submitted + " " + show_id
			+ " completed:  " + comp);
	}

	public String getShowID() {
		return(show_id);
	}

	public java.util.Date getSubmittedDate() {
		return(submitted);
	}

	public String getSubittedTo() {
		return(submitted_to);
	}

	public java.util.Date getCompletedDate() {
		return(completed);
	}

	public boolean isComplete() {
		return(completed!=null);
	}
}
