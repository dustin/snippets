// Copyright (c) 2000  SPY Internetworking <dustin@spy.net>
//
// $Id: TemperatureConf.java,v 1.1 2000/10/15 10:04:49 dustin Exp $

package net.spy.temperature;

import net.spy.SpyConfig;

public class TemperatureConf extends SpyConfig {

	protected String configs[]={
		"/afs/spy.net/misc/web/etc/temperature.conf",
		"/usr/local/etc/temperature.conf",
		"temperature.conf",
		"/tmp/temperature.conf"
	};

	public TemperatureConf() {
		super(); // Thanks for asking
		loadConfig(configs);
	}
}
