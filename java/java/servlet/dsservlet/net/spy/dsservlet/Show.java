// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Show.java,v 1.1 2000/11/06 07:59:28 dustin Exp $

package net.spy.dsservlet;

import java.sql.*;

public class Show extends Object {
	protected String show_id=null;
	protected java.util.Date submitted=null;
	protected String submitted_to=null;
	protected java.util.Date completed=null;

	public Show(ResultSet rs) throws Exception {
		super();

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
