// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestConfig.java,v 1.3 2001/01/27 09:14:04 dustin Exp $

package net.spy.test;

import net.spy.*;

public class TestConfig extends SpyConfig {

	String confs[]={
		"test.conf", "/usr/local/etc/test.conf",
		"/afs/spy.net/misc/web/etc/test.conf"
	};

	public TestConfig() {
		super();
		loadConfig(confs);
	}
}
