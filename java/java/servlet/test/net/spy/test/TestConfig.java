// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestConfig.java,v 1.1 1999/11/15 08:39:27 dustin Exp $

package net.spy.test;

import net.spy.*;

public class TestConfig extends SpyConfig {
	public TestConfig() {
		super("/home/dustin/prog/java/servlet/test/testconfig.xml");
		orput("sweeper_sleep", "10");
		orput("sweeper_maxage", "10");
	}
}
