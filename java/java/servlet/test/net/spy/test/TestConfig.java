// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestConfig.java,v 1.2 2000/05/01 04:32:42 dustin Exp $

package net.spy.test;

import net.spy.*;

public class TestConfig extends SpyConfig {
	public TestConfig() {
		super("/usr/local/etc/testconfig.xml");
		orput("sweeper_sleep", "10");
		orput("sweeper_maxage", "10");
	}
}
