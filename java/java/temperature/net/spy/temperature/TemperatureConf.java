// Copyright (c) 2000  SPY Internetworking <dustin@spy.net>
//
// $Id: TemperatureConf.java,v 1.2 2001/06/01 08:52:03 dustin Exp $

package net.spy.temperature;

import java.io.File;

import net.spy.SpyConfig;

public class TemperatureConf extends SpyConfig {

	private File configs[]={
		new File("/afs/spy.net/misc/web/etc/temperature.conf"),
		new File("/usr/local/etc/temperature.conf"),
		new File("temperature.conf"),
		new File("/tmp/temperature.conf")
	};

	public TemperatureConf() {
		super(); // Thanks for asking
		loadConfig(configs);
	}
}
