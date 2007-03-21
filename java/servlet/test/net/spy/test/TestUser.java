// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestUser.java,v 1.1 1999/11/15 08:39:29 dustin Exp $

package net.spy.test;

import net.spy.*;

public class TestUser extends Object {
	public Integer id=null;
	public String username=null;
	public String password=null;
	public String email=null;
	public String realname=null;
	
	public boolean canAdd(int test_id) {
		return(true);
	}
}
