// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Show.java,v 1.4 2000/11/07 10:49:01 dustin Exp $

package net.spy.dsservlet;

import java.sql.*;
import java.io.*;

import net.spy.*;

public class Show extends Object {
	protected String _show_id=null;
	protected java.util.Date _submitted=null;
	protected String _submitted_to=null;
	protected java.util.Date _completed=null;
	protected long _length=0;

	protected ShowLocator sl=null;

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
		pst.setString(1, _show_id);
		pst.setString(2, _submitted_to);
		pst.executeUpdate();
	}

	public ShowLocator getLocator() {
		if(sl==null) {
			sl=new ShowLocator(_show_id);
		}
		return(sl);
	}

	protected void initFromResultSet(ResultSet rs) throws Exception {
		_show_id=rs.getString("show_id");
		_submitted=rs.getDate("submitted");
		_submitted_to=rs.getString("submitted_to");
		_completed=rs.getTimestamp("completed");
		_length=(long)rs.getInt("length");
	}

	public String toString() {
		String comp=null;
		if(_completed==null) {
			comp="no";
		} else {
			comp=_completed.toString();
		}

		return(_submitted_to + " " + _submitted + " " + _show_id
			+ " completed:  " + comp);
	}

	public String getShowID() {
		return(_show_id);
	}

	public java.util.Date getSubmittedDate() {
		return(_submitted);
	}

	public String getSubittedTo() {
		return(_submitted_to);
	}

	public java.util.Date getCompletedDate() {
		return(_completed);
	}

	public boolean isComplete() {
		return(_completed!=null);
	}

	public long getLength() {
		return(_length);
	}
}
