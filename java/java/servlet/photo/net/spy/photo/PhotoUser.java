// Copyright (c) 1999  Dustin Sallings
//
// $Id: PhotoUser.java,v 1.1 1999/10/20 03:43:04 dustin Exp $

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
}
