// Copyright (c) 1999  Dustin Sallings
//
// $Id: PhotoUser.java,v 1.2 1999/11/26 05:28:28 dustin Exp $

// This class stores an entry from the wwwusers table.

package net.spy.photo;

import net.spy.*;

public class PhotoUser extends Object {
	public Integer id=null;
	public String username=null;
	public String password=null;
	public String email=null;
	public String realname=null;
	public boolean canadd=false;

	// Can this user do administration tasks?
	public boolean canAdmin() {
		boolean ret=false;
		// This is a quick hack implementation until I decide to do it
		// correctly.
		if(username.equals("dustin")) {
			ret=true;
		}
		return(ret);
	}
}
